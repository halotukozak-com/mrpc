package mrpc.derive

import scala.quoted.*

/**
 * The metadata-class-param-driven `materialize` macro — the heart of Phase 10.
 *
 * Unlike the v1 [[MetadataDerivationV1]] (a blind `Done`-walk emitting a fixed flat shape), this macro
 * is driven by the METADATA CLASS `M`'s primary-constructor params: it reads each param's steering
 * annotation, classifies it, and fills it from the real trait's `made.Done` structure plus the engine's
 * shared introspection. The emitted value is `new M[Real](<filled params>)`.
 *
 * Steering vocabulary honored here (Plan 02 slice):
 *   - `@reifyName`                     -> the made `label` (source name) of the current op/param.
 *   - `@reifyName(useRawName = true)`  -> the RESOLVED rpcName ([[RpcName.computeAll]]) — == the engine's.
 *   - `@reifyAnnot` (single)           -> the real annotation instance, read via the made `getAnnotation`
 *                                         idiom (`collectFirst` over `AnnotatedType(_, annot)` where
 *                                         `annot.tpe <:< A`). Slot type `Option[A]` -> `Option`; `A` -> the
 *                                         instance (aborts if absent). `@multi`/`@optional` arity is a Plan-03
 *                                         concern; only `single`/`Option` is wired now.
 *   - `@infer`                         -> `Expr.summon[paramType]`; aborts with the `@infer` clue if absent.
 *   - `@rpcMethodMetadata @multi`      -> one element per op (over [[Matcher.operationTypes]]), each built by
 *                                         recursing this routine in a per-op context; collected into a `List`.
 *   - `@rpcParamMetadata @multi`       -> one element per [[OpReflect.inputElems]] (declaration order), each
 *                                         built in a per-param context; collected into a `List`.
 *
 * No-fork guarantee: resolved rpcNames come from [[RpcName.computeAll]] and arity from
 * [[Matcher.arityTagOf]] — the SAME engine introspection the matcher/dispatcher use. The metadata cannot
 * drift from what the engine dispatches.
 *
 * Pitfall-3 groundwork: the per-element build threads a [[Context]] (the current op / param `Type`), so
 * nested method/param metadata read the RIGHT symbol. Full `@composite` (recurse against the SAME context)
 * lands in Plan 03.
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
    /** A single RPC parameter. */
    case Param(param: OpReflect.Param)

  def impl[M[_]: Type, Real: Type](using Quotes): Expr[M[Real]] =
    import quotes.reflect.*

    val done = Matcher.summonDone[Real]
    val ops: List[Type[?]] = Matcher.operationTypes[Real](done) // refined op types, Done order
    val names: List[String] = RpcName.computeAll(ops) // resolved rpcNames — SAME as the engine

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

    val argExprs: List[Term] = termParams.map { p =>
      val paramTpe = caseFieldsByName.get(p.name) match
        case Some(field) => metaTpe.memberType(field)
        case None => metaTpe.memberType(p)
      fillParam(p, paramTpe, ctx).asTerm
    }

    // new MetaTpe[targs](args): build `New` on the UNAPPLIED class, TypeApply the ctor with the type
    // args, then Apply the value args. (A New whose type tree is already applied makes the ctor look
    // parameterless to a subsequent Select, hence the explicit TypeApply here.)
    val typeArgs: List[TypeRepr] = metaTpe match
      case AppliedType(_, args) => args
      case _ => Nil
    val newSelect = Select(New(TypeIdent(clsSym)), ctor)
    val ctorRef =
      if typeArgs.isEmpty then newSelect
      else TypeApply(newSelect, typeArgs.map(t => TypeTree.of(using t.asType)))
    Apply(ctorRef, argExprs).asExprOf[Any]

  /** Classifies a single ctor param by its steering annotation and builds its value Expr. */
  private def fillParam(using
    Quotes,
  )(param: quotes.reflect.Symbol, paramTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    import quotes.reflect.*

    def has[A: Type]: Boolean = param.annotations.exists(_.tpe <:< TypeRepr.of[A])
    def annotOf[A: Type]: Option[Term] = param.annotations.find(_.tpe <:< TypeRepr.of[A])

    if has[mrpc.annotation.reifyName] then
      val useRaw = annotOf[mrpc.annotation.reifyName].exists(reifyNameUseRaw)
      Expr(reifyName(ctx, useRaw))
    else if has[mrpc.annotation.reifyAnnot] then reifyAnnot(paramTpe, ctx)
    else if has[mrpc.annotation.infer] then infer(param, paramTpe)
    else if has[mrpc.annotation.rpcMethodMetadata] then rpcMethodMetadata(paramTpe, ctx)
    else if has[mrpc.annotation.rpcParamMetadata] then rpcParamMetadata(paramTpe, ctx)
    else
      report.errorAndAbort(
        s"metadata param '${param.name}' has no recognized steering annotation " +
          "(@reifyName/@reifyAnnot/@infer/@rpcMethodMetadata/@rpcParamMetadata)",
      )

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
        p.label // params have no resolved-name distinction; label is the name either way
      case Context.Trait(_, _) =>
        report.errorAndAbort("@reifyName is not valid at the trait level (no single name)")

  /**
   * `@reifyAnnot` (single): finds the param's annotation type `A` in the current context's captured
   * `Metadata` via the made `getAnnotationImpl` idiom (`collectFirst` over `AnnotatedType(_, annot)`
   * where `annot.tpe <:< A`). Slot `Option[A]` -> `Option`; slot `A` -> the instance (aborts if absent).
   */
  private def reifyAnnot(using
    Quotes,
  )(paramTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    import quotes.reflect.*

    val entries: List[Type[?]] = ctx match
      case Context.Method(opType, _) => OpReflect.metadataEntries(opType)
      case Context.Param(p) => p.metadataEntries
      case Context.Trait(_, _) =>
        report.errorAndAbort("@reifyAnnot is not valid at the trait level")

    // Is the slot Option[A] or a bare A?
    val (isOption, annotRepr) = paramTpe.asType match
      case '[Option[a]] => (true, TypeRepr.of[a])
      case _ => (false, paramTpe)

    val found: Option[Term] = entries.iterator
      .map(t => TypeRepr.of(using t))
      .collectFirst { case AnnotatedType(_, annot) if annot.tpe <:< annotRepr => annot }

    if isOption then
      annotRepr.asType match
        case '[a] =>
          found match
            case Some(term) => '{ Some(${ term.asExprOf[a] }) }
            case None => '{ None }
    else
      found match
        case Some(term) => term.asExpr
        case None =>
          report.errorAndAbort(s"@reifyAnnot: no annotation ${annotRepr.show} present on the RPC element")

  /** `@infer`: implicit search for the param's declared type; aborts with the `@infer` clue if absent. */
  private def infer(using
    Quotes,
  )(param: quotes.reflect.Symbol, paramTpe: quotes.reflect.TypeRepr): Expr[Any] =
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
   * `@rpcMethodMetadata @multi`: one element per op (over `Matcher.operationTypes`), each built by
   * recursing `buildValue` in a per-op [[Context.Method]]; collected into a `List`. The element type is
   * the slot's `List[E]` element type.
   */
  private def rpcMethodMetadata(using
    Quotes,
  )(paramTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    import quotes.reflect.*
    val (ops, names) = ctx match
      case Context.Trait(o, n) => (o, n)
      case _ => report.errorAndAbort("@rpcMethodMetadata is only valid at the trait level")

    val elemTpe = listElementType(paramTpe, "@rpcMethodMetadata")
    val elems: List[Expr[Any]] = ops.zip(names).map { (opTpe, name) =>
      buildValue(specialize(elemTpe, opTpe), Context.Method(opTpe, name))
    }
    listOf(elemTpe, elems)

  /**
   * `@rpcParamMetadata @multi`: one element per `OpReflect.inputElems` (declaration order), each built
   * in a per-param [[Context.Param]]; collected into a `List`.
   */
  private def rpcParamMetadata(using
    Quotes,
  )(paramTpe: quotes.reflect.TypeRepr, ctx: Context): Expr[Any] =
    import quotes.reflect.*
    val opType = ctx match
      case Context.Method(o, _) => o
      case _ => report.errorAndAbort("@rpcParamMetadata is only valid within a method context")

    val elemTpe = listElementType(paramTpe, "@rpcParamMetadata")
    val elems: List[Expr[Any]] = OpReflect.inputElems(opType).map { p =>
      buildValue(specialize(elemTpe, p.tpe), Context.Param(p))
    }
    listOf(elemTpe, elems)

  /**
   * Builds a `List[E]` Expr from element Exprs, where `E` is the slot's DECLARED element type (a
   * wildcard like `MethodMeta[?]`). Each element is built at a more-specific type (`MethodMeta[op]`),
   * so it conforms; we ascribe the list to `E` via `List.apply[E]`.
   */
  private def listOf(using
    Quotes,
  )(elemTpe: quotes.reflect.TypeRepr, elems: List[Expr[Any]]): Expr[Any] =
    import quotes.reflect.*
    elemTpe.asType match
      case '[e] =>
        val terms = elems.map(_.asTerm)
        Expr.ofList(terms.map(_.asExprOf[e])).asExprOf[List[e]]

  /** Extracts `E` from a `List[E]` slot type, aborting if the slot is not a `List`. */
  private def listElementType(using
    Quotes,
  )(paramTpe: quotes.reflect.TypeRepr, marker: String): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    paramTpe.asType match
      case '[List[e]] => TypeRepr.of[e]
      case _ =>
        report.errorAndAbort(s"$marker @multi slot must be a List[_]; got ${paramTpe.show}")

  /**
   * Re-specializes a metadata element type's TYPE PARAMETER to the real type it describes. A fixture
   * slot is declared `List[MethodMeta[?]]`; we build each element as `MethodMeta[opType]` (or
   * `ParamMeta[paramType]`) so the `@infer` slot summons against the right `T`. When the element type
   * is not applied (no type param) it is returned unchanged.
   */
  private def specialize(using
    Quotes,
  )(elemTpe: quotes.reflect.TypeRepr, real: Type[?]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    elemTpe match
      case AppliedType(tycon, _ :: Nil) => tycon.appliedTo(TypeRepr.of(using real))
      case _ => elemTpe
