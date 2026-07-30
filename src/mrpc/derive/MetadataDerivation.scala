package mrpc.derive

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
    case Trait(ops: List[Type[?]], resolvedNames: List[String])

    /** A single RPC method/op (its refined `DoneOperation` type + resolved rpcName). */
    case Method(opType: Type[?], resolvedName: String)

    /** A single RPC parameter (a refined [[OpReflect.Param]] type). */
    case Param(param: Type[? <: mrpc.derive.Param])

  def impl[M[_]: Type, Real: Type](using Quotes): Expr[M[Real]] =
    import quotes.reflect.*

    val done = Matcher.summonDone[Real]
    val ops: List[Type[?]] = Matcher.operationTypes[Real](done) // refined op types, Done order
    // Resolved rpcNames come from the type-level `RpcNames[Real].Names` (one authority), read back as
    // values here — SAME order and result as the engine's `RpcName.computeAll`.
    val names: List[String] = RpcNames.namesOf[Real](RpcNames.summonNames[Real])

    val metaTpe: TypeRepr = TypeRepr.of[M[Real]]
    buildValue(metaTpe, Context.Trait(ops, names)).asExprOf[M[Real]]

  /**
   * Builds `new MetaTpe(<filled params>)` by classifying each primary-constructor param of `metaTpe`
   * against the given real-symbol [[Context]].
   */
  private def buildValue(using Quotes)(metaTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    import quotes.reflect.*

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
      fillParam(p, paramTpe, ctx)
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
  private def fillParam(using Quotes)(param: quotes.reflect.Symbol, paramTpe: quotes.reflect.TypeRepr, ctx: Context)
    : Expr[Any] =
    import quotes.reflect.*

    def has[A: Type]: Boolean = param.annotations.exists(_.tpe <:< TypeRepr.of[A])
    def annotOf[A: Type]: Option[Term] = param.annotations.find(_.tpe <:< TypeRepr.of[A])

    // The arity marker steering collection / single / optional slot shapes. Default is @single.
    val arity = arityOf(param)

    if has[mrpc.annotation.composite] then composite(paramTpe, ctx)
    else if has[mrpc.annotation.reifyName] then
      val useRaw = annotOf[mrpc.annotation.reifyName].exists(reifyNameUseRaw)
      Expr(reifyName(ctx, useRaw))
    else if has[mrpc.annotation.reifyAnnot] then reifyAnnot(paramTpe, ctx, arity)
    else if has[mrpc.annotation.infer] then infer(param, paramTpe)
    else if has[mrpc.annotation.rpcMethodMetadata] then rpcMethodMetadata(param, paramTpe, ctx, arity)
    else if has[mrpc.annotation.rpcParamMetadata] then rpcParamMetadata(param, paramTpe, ctx, arity)
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
  private def composite(using Quotes)(paramTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    buildValue(paramTpe, ctx)

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
  private def reifyName(using Quotes)(ctx: Context, useRaw: Boolean): String =
    import quotes.reflect.*
    ctx match
      case Context.Method(opType, resolvedName) =>
        if useRaw then resolvedName else OpReflect.labelOf(opType)
      case Context.Param(p) =>
        OpReflect.labelOf(p) // params have no resolved-name distinction; label is the name either way
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
  private def reifyAnnot(using Quotes)(paramTpe: quotes.reflect.TypeRepr, ctx: Context, arity: SlotArity): Expr[Any] =
    import quotes.reflect.*

    val entries: List[Type[?]] = ctx match
      case Context.Method(opType, _) => OpReflect.metadataEntries(opType)
      case Context.Param(p) => OpReflect.metadataEntries(p)
      case Context.Trait(_, _) =>
        report.errorAndAbort("@reifyAnnot is not valid at the trait level")

    def allTerms(annotRepr: TypeRepr): List[Term] =
      entries.iterator
        .map(t => TypeRepr.of(using t))
        .collect { case AnnotatedType(_, annot) if annot.tpe <:< annotRepr => annot }
        .toList

    // Resolve the slot's annotation element type from its declared shape (List[A]/Option[A]/A),
    // cross-checked against the steering arity marker.
    (arity, paramTpe.asType) match
      case (SlotArity.Multi, '[List[a]]) =>
        // collect-all: every annotation of type A on the symbol.
        val terms = allTerms(TypeRepr.of[a])
        Expr.ofList(terms.map(_.asExprOf[a])).asExprOf[List[a]]
      case (SlotArity.Multi, _) =>
        // @multi over a non-List @reifyAnnot slot is the declared annotation type collected directly.
        val terms = allTerms(paramTpe)
        listOf(paramTpe, terms.map(_.asExpr))
      case (SlotArity.Optional, '[Option[a]]) =>
        allTerms(TypeRepr.of[a]).headOption match
          case Some(term) => '{ Some(${ term.asExprOf[a] }) }
          case None => '{ None }
      case (_, '[Option[a]]) =>
        // a bare Option[A] slot is treated as optional regardless of the marker.
        allTerms(TypeRepr.of[a]).headOption match
          case Some(term) => '{ Some(${ term.asExprOf[a] }) }
          case None => '{ None }
      case (_, _) =>
        // @single (or default): exactly one; abort if absent.
        allTerms(paramTpe) match
          case term :: _ => term.asExpr
          case Nil =>
            report.errorAndAbort(
              s"@single @reifyAnnot: no annotation ${paramTpe.show} present on the RPC element",
            )

  /** `@infer`: implicit search for the param's declared type; aborts with the `@infer` clue if absent. */
  private def infer(using Quotes)(param: quotes.reflect.Symbol, paramTpe: quotes.reflect.TypeRepr): Expr[Any] =
    import quotes.reflect.*
    Implicits.search(paramTpe) match
      case ok: ImplicitSearchSuccess => ok.tree.asExpr
      case _ =>
        val clue = param.annotations
          .find(_.tpe <:< TypeRepr.of[mrpc.annotation.infer])
          .flatMap(inferClue)
          .filter(_.nonEmpty)
          .map(c => s" ($c)")
          .getOrElse("")
        report.errorAndAbort(s"@infer: no given instance for ${paramTpe.show}$clue")

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
  private def rpcMethodMetadata(
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    paramTpe: quotes.reflect.TypeRepr,
    ctx: Context,
    arity: SlotArity,
  ): Expr[Any] =
    import quotes.reflect.*
    val (ops, names) = ctx match
      case Context.Trait(o, n) => (o, n)
      case _ => report.errorAndAbort("@rpcMethodMetadata is only valid at the trait level")

    val items: List[(Type[?], String)] = ops.zip(names)
    collectionSlot(
      param,
      paramTpe,
      arity,
      "@rpcMethodMetadata",
      items,
      key = it => it._2,
      elem = (elemTpe, it) => buildValue(specialize(elemTpe, it._1), Context.Method(it._1, it._2)),
    )

  /**
   * `@rpcParamMetadata`: projects over the op's `inputElems` (declaration order). Same arity shaping as
   * [[rpcMethodMetadata]]; `Map` slots are keyed by paramName.
   */
  private def rpcParamMetadata(
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    paramTpe: quotes.reflect.TypeRepr,
    ctx: Context,
    arity: SlotArity,
  ): Expr[Any] =
    import quotes.reflect.*
    val opType = ctx match
      case Context.Method(o, _) => o
      case _ => report.errorAndAbort("@rpcParamMetadata is only valid within a method context")

    val items: List[Type[? <: Param]] = OpReflect.inputElems(opType)
    collectionSlot(
      param,
      paramTpe,
      arity,
      "@rpcParamMetadata",
      items,
      key = p => OpReflect.labelOf(p),
      elem = (elemTpe, p) =>
        val paramType = p.runtimeChecked match
          case '[Param { type ParamType = t }] => Type.of[t]
        buildValue(specialize(elemTpe, paramType), Context.Param(p)),
    )

  /**
   * Shapes a per-arity projection slot from a list of real items. Shared by `@rpcMethodMetadata` /
   * `@rpcParamMetadata`. `key` resolves a `Map` key from an item; `elem` builds the metadata Expr for
   * an item at the slot's element type. `@single`/`@optional` arity-count mismatches are compile errors.
   */
  private def collectionSlot[I](
    using Quotes,
  )(
    param: quotes.reflect.Symbol,
    paramTpe: quotes.reflect.TypeRepr,
    arity: SlotArity,
    marker: String,
    items: List[I],
    key: I => String,
    elem: (quotes.reflect.TypeRepr, I) => Expr[Any],
  ): Expr[Any] =
    import quotes.reflect.*
    arity match
      case SlotArity.Multi =>
        paramTpe.asType match
          case '[Map[String, e]] =>
            val elemTpe = TypeRepr.of[e]
            val entries: List[Expr[(String, e)]] = items.map { it =>
              val k = Expr(key(it))
              val v = elem(elemTpe, it).asExprOf[e]
              '{ ($k, $v) }
            }
            '{ Map(${ Varargs(entries) }*) }
          case '[List[e]] =>
            val elemTpe = TypeRepr.of[e]
            listOf(elemTpe, items.map(it => elem(elemTpe, it)))
          case _ =>
            report.errorAndAbort(s"$marker @multi slot must be a List[_] or Map[String, _]; got ${paramTpe.show}")
      case SlotArity.Optional =>
        paramTpe.asType match
          case '[Option[e]] =>
            val elemTpe = TypeRepr.of[e]
            items match
              case Nil => '{ None }
              case it :: Nil => '{ Some(${ elem(elemTpe, it).asExprOf[e] }) }
              case _ =>
                report.errorAndAbort(
                  s"$marker @optional slot '${param.name}' matched ${items.size} elements; expected 0 or 1",
                )
          case _ =>
            report.errorAndAbort(s"$marker @optional slot must be an Option[_]; got ${paramTpe.show}")
      case SlotArity.Single =>
        items match
          case it :: Nil => elem(paramTpe, it)
          case other =>
            report.errorAndAbort(
              s"$marker @single slot '${param.name}' requires exactly one match; got ${other.size}",
            )

  /**
   * Builds a `List[E]` Expr from element Exprs, where `E` is the slot's DECLARED element type (a
   * wildcard like `MethodMeta[?]`). Each element is built at a more-specific type (`MethodMeta[op]`),
   * so it conforms; we ascribe the list to `E` via `List.apply[E]`.
   */
  private def listOf(using Quotes)(elemTpe: quotes.reflect.TypeRepr, elems: List[Expr[Any]]): Expr[Any] =
    import quotes.reflect.*
    elemTpe.asType match
      case '[e] =>
        val terms = elems.map(_.asTerm)
        Expr.ofList(terms.map(_.asExprOf[e])).asExprOf[List[e]]

  /**
   * Re-specializes a metadata element type's TYPE PARAMETER to the real type it describes. A fixture
   * slot is declared `List[MethodMeta[?]]`; we build each element as `MethodMeta[opType]` (or
   * `ParamMeta[paramType]`) so the `@infer` slot summons against the right `T`. When the element type
   * is not applied (no type param) it is returned unchanged.
   */
  private def specialize(using Quotes)(elemTpe: quotes.reflect.TypeRepr, real: Type[?]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    elemTpe match
      case AppliedType(tycon, _ :: Nil) => tycon.appliedTo(TypeRepr.of(using real))
      case _ => elemTpe
