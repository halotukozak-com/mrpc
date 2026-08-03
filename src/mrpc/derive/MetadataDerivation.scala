package mrpc.derive

import made.*

import scala.annotation.Annotation
import scala.quoted.{Expr, Quotes, Type}

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
 *   - `@reifyName(useRawName = true)`  -> the RESOLVED rpcName ([[RpcNames]]'s resolution) — == the engine's.
 *   - `@reifyAnnot` (arity-shaped)     -> the real annotation instance(s), read via the made `getAnnotation`
 *                                         idiom. `@single`/`A` -> the instance (aborts if absent);
 *                                         `@optional`/`Option[A]` -> `Option`; `@multi`/`List[A]` -> ALL
 *                                         matching annotations on the symbol (the mrpc-owned collect-all walk).
 *   - `@infer`                         -> `Implicits.search[paramType]`; aborts with the `@infer` clue if absent.
 *   - `@rpcMethodMetadata` (arity)     -> projects over the trait's ops (`Context.Trait`'s captured
 *                                         `Ops`/`Names`): `@multi` -> `List`/`Map[String, _]` (keyed by
 *                                         rpcName); `@optional` -> `Option`; `@single` -> exactly one
 *                                         (compile error on 0/>1).
 *   - `@rpcParamMetadata` (arity)      -> projects over the op's `InputElems` (declaration order, via
 *                                         [[OpReflect]]/[[TupleTraverse]]), same arity shaping; `Map`
 *                                         slots keyed by paramName.
 *
 * No-fork guarantee: resolved rpcNames come from [[RpcNames]] and the op set from `Done.Of[Real]` —
 * the SAME engine introspection the matcher/dispatcher use. The metadata cannot drift from what the
 * engine dispatches.
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
  sealed trait Context
  object Context:
    /** The whole real trait: ops + their resolved names, in `Done` order. */
    sealed abstract class Trait extends Context:
      type Ops <: Tuple /* of DoneOperation */

      type Names <: Tuple /* of String */

      def operations: Ops

    object Trait:
      def apply[names <: Tuple](ops: Tuple)(using ops.type containsOnly DoneOperation, names containsOnly String)
        : Trait { type Ops = ops.type; type Names = names } = new Trait:
        override type Names = names
        override type Ops = ops.type
        override def operations: Ops = ops.asInstanceOf[Ops]

    /** A single RPC method/op (its refined `DoneOperation` type + resolved rpcName). */
    sealed abstract class Method extends Context:
      type Op <: DoneOperation
      type Name <: String
      val op: Op

    object Method:
      def apply[name <: String](operation: DoneOperation): Method { type Op = operation.type; type Name = name } =
        new Method:
          override type Op = operation.type
          override type Name = name

          val op: operation.type = operation

    /** A single RPC parameter (a refined [[Param]] type). */
    sealed abstract class Param extends Context:
      type P <: mrpc.derive.Param

      val underlying: P

    object Param:
      def apply(p: mrpc.derive.Param): Param { type P = p.type } = new Param:
        override type P = p.type
        override val underlying: p.type = p

  inline def impl[M[_], Real, Names <: Tuple](
    operations: Tuple,
  )(using
    operations.type containsOnly DoneOperation,
    Names containsOnly String,
  )(using
    made: Made.Of[M[Real]],
  ) =
    val ctx = Context.Trait[Names](operations)
    buildValue[M[Real]](using made, ctx)

  inline def buildValue[M: Made.Of as made](using Context): M =
    val p = made.asInstanceOf[Made.ProductOf[M]]
    val elems = fillAllParams(made.elems).asInstanceOf[p.ElemTypes] // todo: can we avoid this asIsntanceOf?
    p.fromTuple(elems)

  inline private def fillAllParams(inline acc: Tuple)(using Context): Tuple = inline acc match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: tail) =>
      fillParam(acc.head.asInstanceOf[h & MadeElem]) *: fillAllParams(acc.tail) // todo: do it better

  inline private def fillParam(e: MadeElem)(using ctx: Context) =
    import made.{getAnnotation, hasAnnotation}

    val arity = if e.hasAnnotation[mrpc.annotation.multi] then SlotArity.Multi
    else if e.hasAnnotation[mrpc.annotation.optional] then SlotArity.Optional
    else SlotArity.Single

    if e.hasAnnotation[mrpc.annotation.composite] then composite[e.Type]
    else if e.hasAnnotation[mrpc.annotation.reifyName] then
      val useRaw = e.getAnnotation[mrpc.annotation.reifyName].exists(_.useRawName)
      reifyName(useRaw)
    else if e.hasAnnotation[mrpc.annotation.reifyAnnot] then reifyAnnot[e.Type](arity)
    else if e.hasAnnotation[mrpc.annotation.rpcMethodMetadata] then
      val ev = compiletime.summonInline[ctx.type <:< Context.Trait]
      rpcMethodMetadata[e.Type](e.label, arity)(using ev(ctx))
    else if e.hasAnnotation[mrpc.annotation.rpcParamMetadata] then
      val ev = compiletime.summonInline[ctx.type <:< Context.Method]
      rpcParamMetadata[e.Type](e.label, arity)(using ev(ctx))
    else inline if e.hasAnnotation[mrpc.annotation.infer] then compiletime.summonInline[e.Type]
    //                .getOrElse:
    //                  val clue = Option(annot.clue).filter(_.nonEmpty).map(c => s" ($c)").getOrElse("")
    //                  report.errorAndAbort(s"@infer: no given instance for ${Type.show[Param]}$clue")
    else
      compiletime.error(
        "metadata param '" + compiletime.constValue[e.Label] + "' has no recognized steering annotation " +
          "(@composite/@reifyName/@reifyAnnot/@infer/@rpcMethodMetadata/@rpcParamMetadata)",
      )

  /**
   * The collection-arity marker on a metadata param. `@multi` -> a collection slot; `@optional` ->
   * an `Option` slot; absent (or `@single`) -> exactly-one. Mirrors commons `single`/`optional`/`multi`.
   */
  private enum SlotArity:
    case Single, Optional, Multi

  /**
   * `@composite`: the param's type C is a class with a public primary ctor; recurse `buildValue(C, ctx)`
   * threading the SAME real-symbol context, so C's `@reifyName` reads the enclosing real symbol (the op
   * or param), NOT C's field names (research Pitfall 3). Emits `new C(<filled params>)`.
   */
  inline private def composite[M](using ctx: Context): M =
    buildValue[M]

  /** `@reifyName`: source label or, when `useRaw`, the resolved rpcName for the current context. */
  inline private def reifyName(inline useRaw: Boolean)(using ctx: Context): String =
    inline ctx match
      case ctx: Context.Method =>
        inline if useRaw then compiletime.constValue[ctx.Name]
        else compiletime.constValue[ctx.op.Label]
      case ctx: Context.Param =>
        compiletime.constValue[ctx.underlying.Label]
      case ctx: Context.Trait => compiletime.error("@reifyName is not valid at the trait level (no single name)")

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
  inline private def reifyAnnot[Param](arity: SlotArity)(using ctx: Context): Any =

    def allTerms[A]: List[A] = inline ctx match
      case ctx: Context.Method => getAllAnnotations(ctx.op)(using containsOnly.refl)[A & Annotation]
      case ctx: Context.Param => getAllAnnotations(ctx.underlying)(using containsOnly.refl)[A & Annotation]
      case _: Context.Trait => compiletime.error("@reifyAnnot is not valid at the trait level")

    // Resolve the slot's annotation element type from its declared shape (List[A]/Option[A]/A),
    // cross-checked against the steering arity marker.
    (arity, compiletime.erasedValue[Param]) match
      case (SlotArity.Multi, _: List[a]) =>
        // collect-all: every annotation of type A on the symbol.
        allTerms[a]
      case (SlotArity.Multi, _) =>
        // @multi over a non-List @reifyAnnot slot is the declared annotation type collected directly.
        allTerms[Param]
      case (SlotArity.Optional, _: Option[a]) =>
        allTerms[a].headOption
      case (_, _: Option[a]) =>
        // a bare Option[A] slot is treated as optional regardless of the marker.
        allTerms[a].headOption
      case (_, _) =>
        // @single (or default): exactly one; abort if absent.
        allTerms[Param] match
          case term :: _ => term
          case Nil =>
            compiletime.error(
              s"@single @reifyAnnot: no annotation + "
              /** ${Type.show[Param]}  * */
              + "present on the RPC element",
            )

  /**
   * `@rpcMethodMetadata`: projects over the trait's ops. Arity-shaped:
   *   - `@multi List[E]`             -> one element per op, collected into a `List`.
   *   - `@multi Map[String, E]`      -> one element per op, keyed by resolved rpcName.
   *   - `@optional Option[E]`        -> the single op if exactly one, else `None`.
   *   - `@single E` (or default)     -> the single op; `report.errorAndAbort` on 0 or >1 (research Pitfall 4).
   * Each element recurses `buildValue` in a per-op [[Context.Method]].
   */

  inline private def buildAllElems[e, name <: String](inline acc: Tuple): Tuple = inline acc match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: tail) =>
      buildElem[e, h & DoneOperation](using Context.Method[name](acc.head.asInstanceOf[h & DoneOperation])) *:
        buildAllElems(acc.tail) // todo: do it better

  inline private def buildAllElems2[e](inline acc: Tuple): Tuple = inline acc match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: tail) =>
      buildElem[e, h & Param](using Context.Param(acc.head.asInstanceOf[h & Param])) *: buildAllElems2(acc.tail) // todo: do it better

  inline def buildElem[elem <: AnyKind, T](using ctx: Context) = ${ buildElemImpl[elem, T]('ctx) }

  private def buildElemImpl[elem <: AnyKind: Type, T: Type](
    ctx: Expr[Context],
  )(using Quotes,
  ): Expr[?] = Type.of[elem] match
    case '[type f[_] <: Any; f] =>
      '{ buildValue[f[T]](using compiletime.summonInline[Made.Of[f[T]]], $ctx) }
    case '[type e <: Any; e] =>
      '{ buildValue[e](using compiletime.summonInline[Made.Of[e]], $ctx) }

  inline private def rpcMethodMetadata[P](paramName: String, arity: SlotArity)(using ctx: Context.Trait): Any =
    val names = compiletime.constValueTuple[ctx.Names]
    val items = ctx.operations

    inline arity match
      case SlotArity.Multi =>
        inline compiletime.erasedValue[P] match
          case _: Map[String, e] =>
            NamedTuple.build[names.type]()(buildAllElems(items)).toList.asInstanceOf[List[(String, ?)]].toMap
          case _: List[e] =>
            buildAllElems(items).toList
      case SlotArity.Optional =>
        inline compiletime.erasedValue[P] match
          case _: Option[e] =>
            inline items match
              case _: EmptyTuple => None
              case (it: DoneOperation) :: Nil => Some(buildElem[e, it.Type])
              case _ =>
                compiletime.error(
                  s"@rpcMethodMetadat @optional slot '$paramName' matched ${items.size} elements; expected 0 or 1",
                )
      case SlotArity.Single =>
        inline items match
          case (it: DoneOperation) :: Nil => buildElem[P, it.Type]
          case other =>
            compiletime.error(
              s"@rpcMethodMetadata @single slot '$paramName' requires exactly one match; got ${other.size}",
            )

  /**
   * `@rpcParamMetadata`: projects over the op's `inputElems` (declaration order). Same arity shaping as
   * [[rpcMethodMetadata]]; `Map` slots are keyed by paramName.
   */
  inline private def rpcParamMetadata[P](
    paramName: String,
    arity: SlotArity,
  )(using ctx: Context.Method,
  ): P =
    val names = compiletime.constValueTuple[Tuple.Map[ctx.op.InputElems, InputElem.ExtractLabel]]
    val items = ctx.op.inputElems

    (inline arity match
      case SlotArity.Multi =>
        inline compiletime.erasedValue[P] match
          case _: Map[String, e] =>
            NamedTuple.build[names.type]()(buildAllElems(items)).toList.asInstanceOf[List[(String, ?)]].toMap
          case _: List[e] =>
            buildAllElems(items).toList
      case SlotArity.Optional =>
        inline compiletime.erasedValue[P] match
          case _: Option[e] =>
            inline items match
              case _: EmptyTuple => None
              case (it: Param) :: Nil => Some(buildElem[e, it.ParamType](using Context.Param(it)))
              case _ =>
                compiletime.error(
                  s"@rpcParamMetadata @optional slot '$paramName' matched ${items.size} elements; expected 0 or 1",
                )
      case SlotArity.Single =>
        inline items match
          case (it: Param) :: Nil => buildElem[P, it.ParamType](using Context.Param(it))
          case other =>
            compiletime.error(
              s"@rpcParamMetadata @single slot '$paramName' requires exactly one match; got ${other.size}",
            )
    ).asInstanceOf[P]
