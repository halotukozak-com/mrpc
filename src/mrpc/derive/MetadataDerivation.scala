package mrpc.derive

import made.{Done, DoneOperation, InputElem}

import scala.quoted.*

/**
 * The metadata-class-param-driven `materialize` macro — the heart of Phase 10.
 *
 * Unlike a blind `Done`-walk emitting a fixed flat shape, this macro
 * is driven by the METADATA CLASS `M`'s primary-constructor params: it reads each param's steering
 * annotation, classifies it, and fills it from the real trait's `made.Done` structure plus the engine's
 * shared introspection. The emitted value is `new M[Real](<filled params>)`.
 *
 * Steering vocabulary honored:
 *   - `@composite`                     -> recurse `buildValue` over the param type's primary ctor against
 *                                         the SAME real-symbol context (NameInfo flattening; Pitfall 3).
 *   - `@reifyName`                     -> the made `label` (source name) of the current op/param.
 *   - `@reifyName(useRawName = true)`  -> the RESOLVED rpcName ([[RpcName.computeAll]]) — == the engine's.
 *   - `@reifyAnnot` (arity-shaped)     -> the real annotation instance(s), read via the made `getAnnotation`
 *                                         idiom. `@single`/`A` -> the instance (aborts if absent);
 *                                         `@optional`/`Option[A]` -> `Option`; `@multi`/`List[A]` -> ALL
 *                                         matching annotations on the symbol (the mrpc-owned collect-all walk).
 *   - `@infer`                         -> `Implicits.search[paramType]`; aborts with the `@infer` clue if absent.
 *   - `@rpcMethodMetadata` (arity)     -> projects over [[Matcher.operationTypes]]: `@multi` -> `List`/
 *                                         `Map[String, _]` (keyed by rpcName); `@optional` -> `Option`;
 *                                         `@single` -> exactly one (compile error on 0/>1).
 *   - `@rpcParamMetadata` (arity)      -> projects over [[OpReflect.inputElems]] (declaration order), same
 *                                         arity shaping; `Map` slots keyed by paramName.
 *
 * No-fork guarantee: resolved rpcNames come from [[RpcName.computeAll]] and the op set from
 * [[Matcher.operationTypes]] — the SAME engine introspection the matcher/dispatcher use. The metadata
 * cannot drift from what the engine dispatches.
 *
 * Pitfall-3 (`@composite`): the per-element build threads a [[Context]] (the current op / param `Type`),
 * and `@composite` recurses with the SAME `Context` so the nested class's `@reifyName` reads the real
 * symbol, not its own field names.
 */
private[mrpc] object MetadataDerivation:

  /**
   * The real-symbol context a metadata value is being built against. Determines what `@reifyName`,
   * `@reifyAnnot`, `@rpcMethodMetadata`, and `@rpcParamMetadata` resolve to.
   */
  private enum Context:
    /** The whole real trait: ops + their resolved names, in `Done` order. */
    case Trait(ops: List[Type[?]], resolvedNames: List[Type[? <: String]])

    /** A single RPC method/op (its refined `DoneOperation` type + resolved rpcName). */
    case Method(opType: Type[?], resolvedName: Type[? <: String])

    /** A single RPC parameter (a refined [[OpReflect.Param]] type). */
    case Param(param: Type[? <: mrpc.derive.Param])

  def impl[M[_]: Type, Real: Type](done: Expr[Done.Of[Real]], names: Expr[RpcNames[Real]])(using Quotes)
    : Expr[M[Real]] =

    val ops = done match
      case '{ type operations <: Tuple; $_ : Done { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]
    val values = RpcNames.namesOf[Real](names)

    buildValue[M[Real]](Context.Trait(ops, values)).asExprOf[M[Real]]

  /**
   * Builds `new MetaTpe(<filled params>)` by classifying each primary-constructor param of `metaTpe`
   * against the given real-symbol [[Context]].
   */
  private def buildValue[M: Type](ctx: Context)(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val metaTpe = TypeRepr.of[M]
    val clsSym = metaTpe.typeSymbol
    val ctor = clsSym.primaryConstructor
    val paramSymss = ctor.paramSymss

    // Term-param symbols (skip type-param lists). The case-class fields carry the steering annotations
    // and the (possibly type-param-substituted) member types.
    val termParams: List[Symbol] = paramSymss.filterNot(_.exists(_.isType)).flatten

    // Case fields carry the type-arg-SUBSTITUTED member types (a ctor-param symbol's memberType does
    // NOT substitute the class type args); look each ctor param up by name among the case fields.
    val caseFieldsByName: Map[String, Symbol] = clsSym.caseFields.map(f => f.name -> f).toMap

    val argExprs: List[Expr[Any]] = termParams.map { p =>
      val paramTpe = caseFieldsByName.get(p.name) match
        case Some(field) => metaTpe.memberType(field)
        case None => metaTpe.memberType(p)
      paramTpe.asType match
        case '[param] =>
          fillParam[param](p, ctx)
    }

    // `new MetaTpe[targs](args)` via the compiler-synthesized `Mirror.Product` for `MetaTpe` — no
    // constructor tree to synthesize by hand: `fromProduct` positionally applies `argExprs` (any
    // `Product`, so a plain args tuple suffices) through the class's actual primary constructor.
    metaTpe.asType match
      case '[t] =>
        '{
          scala.compiletime
            .summonInline[scala.deriving.Mirror.ProductOf[t]]
            .fromProduct(${ Expr.ofRefinedTuple(argExprs) })
        }

  /** Classifies a single ctor param by its steering annotation and builds its value Expr. */
  private def fillParam[Param: Type](using Quotes)(param: quotes.reflect.Symbol, ctx: Context): Expr[Any] =
    import quotes.reflect.*

    def has[A: Type]: Boolean = param.annotations.exists(_.tpe <:< TypeRepr.of[A])
    def annotOf[A: Type]: Option[Term] = param.annotations.find(_.tpe <:< TypeRepr.of[A])

    // The arity marker steering collection / single / optional slot shapes. Default is @single.
    val arity = arityOf(param)

    if has[mrpc.annotation.composite] then composite[Param](ctx)
    else if has[mrpc.annotation.reifyName] then
      val useRaw = annotOf[mrpc.annotation.reifyName].exists(reifyNameUseRaw)
      Expr(Type.valueOfConstant(using reifyName(ctx, useRaw)).get)
    else if has[mrpc.annotation.reifyAnnot] then reifyAnnot[Param](ctx, arity)
    else if has[mrpc.annotation.infer] then infer[Param](param)
    else if has[mrpc.annotation.rpcMethodMetadata] then rpcMethodMetadata[Param](param, ctx, arity)
    else if has[mrpc.annotation.rpcParamMetadata] then rpcParamMetadata[Param](param, ctx, arity)
    else
      report.errorAndAbort(
        s"metadata param '${param.name}' has no recognized steering annotation " +
          "(@composite/@reifyName/@reifyAnnot/@infer/@rpcMethodMetadata/@rpcParamMetadata)",
      )

  /**
   * The collection-arity marker on a metadata param. `@multi` -> a collection slot; `@optional` ->
   * an `Option` slot; absent (or `@single`) -> exactly-one. Mirrors commons `single`/`optional`/`multi`.
   */
  private enum SlotArity:
    case Single, Optional, Multi

  private def arityOf(using Quotes)(param: quotes.reflect.Symbol): SlotArity =
    import quotes.reflect.*
    def has[A: Type]: Boolean = param.annotations.exists(_.tpe <:< TypeRepr.of[A])
    if has[mrpc.annotation.multi] then SlotArity.Multi
    else if has[mrpc.annotation.optional] then SlotArity.Optional
    else SlotArity.Single

  /**
   * `@composite`: the param's type C is a class with a public primary ctor; recurse `buildValue(C, ctx)`
   * threading the SAME real-symbol context, so C's `@reifyName` reads the enclosing real symbol (the op
   * or param), NOT C's field names (research Pitfall 3). Emits `new C(<filled params>)`.
   */
  private def composite[M: Type](ctx: Context)(using Quotes): Expr[Any] =
    buildValue[M](ctx)

  /** Reads `@reifyName`'s `useRawName` boolean off the annotation term (default false). */
  private def reifyNameUseRaw(using Quotes)(annot: quotes.reflect.Term): Boolean =
    import quotes.reflect.*
    def collectArgs(t: Term): List[Term] = t match
      case Apply(fun, args) => collectArgs(fun) ++ args
      case TypeApply(fun, _) => collectArgs(fun)
      case _ => Nil
    collectArgs(annot).exists {
      case Literal(BooleanConstant(b)) => b
      case NamedArg(_, Literal(BooleanConstant(b))) => b
      case _ => false
    }

  /** `@reifyName`: source label or, when `useRaw`, the resolved rpcName for the current context. */
  private def reifyName(using Quotes)(ctx: Context, useRaw: Boolean): Type[? <: String] =
    import quotes.reflect.*
    ctx.runtimeChecked match
      case Context
            .Method('[type label <: String; { type Label = label }], '[type resolvedName <: String; resolvedName]) =>
        if useRaw then Type.of[resolvedName] else Type.of[label]
      case Context.Param('[type label <: String; { type Label = label }]) =>
        Type.of[label] // params have no resolved-name distinction; label is the name either way
      case Context.Trait(_, _) =>
        report.errorAndAbort("@reifyName is not valid at the trait level (no single name)")

  /**
   * `@reifyAnnot` (arity-sensitive): finds the param's annotation type `A` in the current context's
   * captured `Metadata` via the made `getAnnotationImpl` idiom (a `<:<` match over `AnnotatedType(_,
   * annot)` entries). Slot shape by arity:
   *   - `@single` (or bare slot `A`)  -> the single instance; `report.errorAndAbort` if absent (the
   *                                      `@single` mismatch surfaced by `MetadataCompileErrorSuite`).
   *   - `@optional` / slot `Option[A]` -> `Some(a)`/`None`.
   *   - `@multi` / slot `List[A]`      -> ALL matching annotations on the symbol (the mrpc-owned
   *                                      tuple walk: collect every `AnnotatedType(_, a)` with `a.tpe <:< A`).
   */
  private def reifyAnnot[Param: Type](ctx: Context, arity: SlotArity)(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val entries: List[Type[?]] = ctx match
      case Context.Method(opType, _) => OpReflect.metadataEntries(opType)
      case Context.Param(p) => OpReflect.metadataEntries(p)
      case Context.Trait(_, _) =>
        report.errorAndAbort("@reifyAnnot is not valid at the trait level")

    def allTerms[A: Type]: List[Expr[A]] =
      entries.iterator
        .map(t => TypeRepr.of(using t))
        .collect { case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A] }
        .toList

    // Resolve the slot's annotation element type from its declared shape (List[A]/Option[A]/A),
    // cross-checked against the steering arity marker.
    (arity, Type.of[Param]) match
      case (SlotArity.Multi, '[List[a]]) =>
        // collect-all: every annotation of type A on the symbol.
        Expr.ofList(allTerms[a]).asExprOf[List[a]]
      case (SlotArity.Multi, _) =>
        // @multi over a non-List @reifyAnnot slot is the declared annotation type collected directly.
        Expr.ofList(allTerms[Param]).asExprOf[List[Param]]
      case (SlotArity.Optional, '[Option[a]]) =>
        Expr.ofOption(allTerms[a].headOption)
      case (_, '[Option[a]]) =>
        // a bare Option[A] slot is treated as optional regardless of the marker.
        Expr.ofOption(allTerms[a].headOption)
      case (_, _) =>
        // @single (or default): exactly one; abort if absent.
        allTerms[Param] match
          case term :: _ => term
          case Nil =>
            report.errorAndAbort(
              s"@single @reifyAnnot: no annotation ${Type.show[Param]} present on the RPC element",
            )

  /** `@infer`: implicit search for the param's declared type; aborts with the `@infer` clue if absent. */
  private def infer[Param: Type](using Quotes)(param: quotes.reflect.Symbol): Expr[Any] =
    import quotes.reflect.*
    Expr
      .summon[Param]
      .getOrElse:
        val clue = param.annotations
          .find(_.tpe <:< TypeRepr.of[mrpc.annotation.infer])
          .flatMap(inferClue)
          .filter(_.nonEmpty)
          .map(c => s" ($c)")
          .getOrElse("")
        report.errorAndAbort(s"@infer: no given instance for ${Type.show[Param]}$clue")

  /** Reads `@infer`'s `clue` string off the annotation term. */
  private def inferClue(using Quotes)(annot: quotes.reflect.Term): Option[String] =
    import quotes.reflect.*
    def collectArgs(t: Term): List[Term] = t match
      case Apply(fun, args) => collectArgs(fun) ++ args
      case TypeApply(fun, _) => collectArgs(fun)
      case _ => Nil
    collectArgs(annot).collectFirst {
      case Literal(StringConstant(s)) => s
      case NamedArg(_, Literal(StringConstant(s))) => s
    }

  /**
   * `@rpcMethodMetadata`: projects over the trait's ops. Arity-shaped:
   *   - `@multi List[E]`             -> one element per op, collected into a `List`.
   *   - `@multi Map[String, E]`      -> one element per op, keyed by resolved rpcName.
   *   - `@optional Option[E]`        -> the single op if exactly one, else `None`.
   *   - `@single E` (or default)     -> the single op; `report.errorAndAbort` on 0 or >1 (research Pitfall 4).
   * Each element recurses `buildValue` in a per-op [[Context.Method]].
   */
  private def rpcMethodMetadata[P: Type](
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    ctx: Context,
    arity: SlotArity,
  ): Expr[Any] =
    import quotes.reflect.*
    val (ops, names) = ctx match
      case Context.Trait(o, n) => (o, n)
      case _ => report.errorAndAbort("@rpcMethodMetadata is only valid at the trait level")

    collectionSlot[(Type[?], Type[? <: String]), P](
      param,
      arity,
      "@rpcMethodMetadata",
      ops.zip(names),
      key = it => it._2,
      elem = {
        case ('[type f[_]; f], ('[real], resolvedName)) =>
          buildValue[f[real]](Context.Method(Type.of[real], resolvedName))
        case ('[elemTpe], ('[real], resolvedName)) =>

          buildValue[elemTpe](Context.Method(Type.of[real], resolvedName))
        case (_, _) => ???
      },
    )

  /**
   * `@rpcParamMetadata`: projects over the op's `inputElems` (declaration order). Same arity shaping as
   * [[rpcMethodMetadata]]; `Map` slots are keyed by paramName.
   */
  private def rpcParamMetadata[P: Type](
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    ctx: Context,
    arity: SlotArity,
  ): Expr[Any] =
    import quotes.reflect.*
    val opType = ctx match
      case Context.Method(o, _) => o
      case _ => report.errorAndAbort("@rpcParamMetadata is only valid within a method context")

    collectionSlot[Type[? <: Param], P](
      param,
      arity,
      "@rpcParamMetadata",
      opType match
        case '[type elems <: Tuple; DoneOperation {
              type InputElems = elems
            }] =>
          TupleTraverse.traverseTuple[elems, InputElem].map(OpReflect.paramOf),
      key = {
        case '[type label <: String; { type Label = label }] =>
          Type.of[label]
        case _ => ???
      },
      elem = {
        case ('[type f[_]; f], p @ '[Param { type ParamType = paramType }]) =>
          buildValue[f[paramType]](Context.Param(p))
        case ('[elemTpe], p) =>
          buildValue[elemTpe](Context.Param(p))
        case _ => ???
      },
    )

  /**
   * Shapes a per-arity projection slot from a list of real items. Shared by `@rpcMethodMetadata` /
   * `@rpcParamMetadata`. `key` resolves a `Map` key from an item; `elem` builds the metadata Expr for
   * an item at the slot's element type. `@single`/`@optional` arity-count mismatches are compile errors.
   */
  private def collectionSlot[I, Param: Type](
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    arity: SlotArity,
    marker: String,
    items: List[I],
    key: I => Type[? <: String],
    elem: (Type[?], I) => Expr[Any], // [T:Type] => I => Expr[T]  ?
  ): Expr[Any] =
    import quotes.reflect.*
    arity match
      case SlotArity.Multi =>
        Type.of[Param] match
          case '[Map[String, e]] =>
            val entries: List[Expr[(String, e)]] = items.map { it =>
              val k = Expr(Type.valueOfConstant(using key(it)).get)
              val v = elem(Type.of[e], it).asExprOf[e]
              '{ ($k, $v) }
            }
            '{ Map(${ Varargs(entries) }*) }
          case '[List[e]] =>
            Expr.ofList(items.map(it => elem(Type.of[e], it)).map(_.asExprOf[e])).asExprOf[List[e]]
          case _ =>
            report.errorAndAbort(s"$marker @multi slot must be a List[_] or Map[String, _]; got ${Type.show[Param]}")
      case SlotArity.Optional =>
        Type.of[Param] match
          case '[Option[e]] =>
            items match
              case Nil => '{ None }
              case it :: Nil => '{ Some(${ elem(Type.of[e], it).asExprOf[e] }) }
              case _ =>
                report.errorAndAbort(
                  s"$marker @optional slot '${param.name}' matched ${items.size} elements; expected 0 or 1",
                )
          case _ =>
            report.errorAndAbort(s"$marker @optional slot must be an Option[_]; got ${Type.show[Param]}")
      case SlotArity.Single =>
        items match
          case it :: Nil => elem(Type.of[Param], it)
          case other =>
            report.errorAndAbort(
              s"$marker @single slot '${param.name}' requires exactly one match; got ${other.size}",
            )
