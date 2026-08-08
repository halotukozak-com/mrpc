package mrpc
package derive

import made.*

import scala.annotation.Annotation
import scala.quoted.{Expr, Quotes, Type}

/**
 * The metadata-class-param-driven `materialize` macro. Unlike a blind `Done`-walk emitting a fixed flat
 * shape, this is driven by the METADATA CLASS `M`'s primary-constructor params: it reads each param's
 * steering annotation, classifies it, and fills it from the real trait's `made.Done` structure. The
 * emitted value is `new M[Real](<filled params>)`.
 *
 * Steering vocabulary honored:
 *   - `@composite`                     -> recurse `buildValue` over the param type's primary ctor,
 *                                         against the SAME real-symbol [[Context]].
 *   - `@reifyName`                     -> the made `label` (source name) of the current op/param.
 *   - `@reifyName(useRawName = true)`  -> the RESOLVED rpcName ([[RpcNames]]'s resolution).
 *   - `@reifyAnnot` (arity-shaped)     -> the real annotation instance(s), read via made's
 *                                         `getAnnotation` idiom. `@single`/`A` -> the instance (aborts
 *                                         if absent); `@optional`/`Option[A]` -> `Option`;
 *                                         `@multi`/`List[A]` -> all matching annotations.
 *   - `@infer`                         -> `Implicits.search[paramType]`; aborts with a clue if absent.
 *   - `@rpcMethodMetadata` (arity)     -> projects over the trait's ops: `@multi` -> `List`/
 *                                         `Map[String, _]` keyed by rpcName; `@optional` -> `Option`;
 *                                         `@single` -> exactly one (compile error on 0/>1).
 *   - `@rpcParamMetadata` (arity)      -> same arity shaping, projected over the op's `InputElems`;
 *                                         `Map` slots keyed by paramName.
 *
 * Resolved rpcNames come from [[RpcNames]] and the op set from `Done.Of[Real]` — the same engine
 * introspection the dispatcher uses, so metadata cannot drift from what the engine dispatches.
 */
private[mrpc] object MetadataDerivation:

  /**
   * The real-symbol context a metadata value is being built against. Determines what `@reifyName`,
   * `@reifyAnnot`, `@rpcMethodMetadata`, and `@rpcParamMetadata` resolve to.
   */

  transparent inline private def ctx(using ctx: Context): ctx.type = ctx

  private sealed trait Context
  private object Context:
    /** The whole real trait: ops + their resolved names, in `Done` order. */
    sealed trait Trait extends Context:
      type Ops <: Tuple /* of DoneOperation */
      type Names <: Tuple /* of String */

      given Ops containsOnly made.DoneOperation = compiletime.deferred
      given Names containsOnly String = compiletime.deferred

      def operations: Ops

    object Trait:
      // `ops` MUST be a genuine type parameter (not a bare `Tuple`-typed value param with `.type`
      // tacked on the result) — a value param declared as plain `Tuple` widens away the caller's
      // per-op refined types (and with them, each op's captured `Metadata`) at the parameter
      // boundary; `ops.type` inside the return type then just resolves back to that widened `Tuple`.
      // A type parameter, inferred from the argument's OWN static type, preserves it end-to-end.
      def apply[ops <: Tuple, names <: Tuple](ops0: ops)(using ops containsOnly DoneOperation, names containsOnly String)
        : Trait { type Ops = ops; type Names = names } = new Trait:
        override type Names = names
        override type Ops = ops
        override def operations: Ops = ops0

    /** A single RPC method/op (its refined `DoneOperation` type + resolved rpcName). */
    sealed abstract class Method extends Context:
      type Op <: DoneOperation
      type Name <: String
      val op: Op

    object Method:
      // Same reasoning as `Trait.apply`: `operation` must be typed as the type parameter `opT`, not
      // the `DoneOperation` supertype, or its real refined type (and captured `Metadata`) is lost.
      def apply[name <: String, opT <: DoneOperation](operation: opT): Method { type Op = opT; type Name = name } =
        new Method:
          override type Op = opT
          override type Name = name

          val op: opT = operation

    /** A single RPC parameter (a refined [[Param]] type). */
    sealed abstract class Param extends Context:
      type P <: InputElem

      val underlying: P

    object Param:
      // Same reasoning again: `p` must be typed as the type parameter `pT`, not `InputElem`.
      def apply[pT <: InputElem](p: pT): Param { type P = pT } = new Param:
        override type P = pT
        override val underlying: pT = p

  inline def impl[M[_], Real, Ops <: Tuple](
    operations: Ops,
    names: Tuple,
  )(using
    Ops containsOnly DoneOperation,
    names.type containsOnly String,
  )(using made: Made.Of[M[Real]],
  ): M[Real] =
    val ctx = Context.Trait[Ops, names.type](operations)
    buildValue[M[Real]](using made, ctx)

  inline private def buildValue[M: Made.Of as made](using Context): M =
    val p = made.asInstanceOf[Made.ProductOf[M]]
    val elems = fillAllParams(made.elems).asInstanceOf[p.ElemTypes] // todo: can we avoid this asInstanceOf?
    p.fromTuple(elems)

  inline private def fillAllParams(elems: Tuple)(using Context)(using elems.type containsOnly MadeElem): Tuple =
    inline elems match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        val head = elems.head.asInstanceOf[head & MadeElem]
        val tail = elems.tail.asInstanceOf[tail & Tuple.Tail[elems.type]]

        realCons(fillParam(head), fillAllParams(tail))

  extension (e: MadeElem) transparent inline private def getUserRawName: Boolean = ${ getUserRawNameImpl[e.Metadata] }

  private def getUserRawNameImpl[M <: Tuple: Type](using quotes: Quotes): Expr[Boolean] =
    import quotes.reflect.*

    def loop[Tup <: Tuple: Type](using Quotes): Expr[Boolean] = Type.of[Tup] match
      case '[EmptyTuple] =>
        report.errorAndAbort("@reifyName: no @reifyName annotation found on the metadata param's type chain")
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[mrpc.annotation.reifyName] =>
            annot.asExprOf[mrpc.annotation.reifyName] match
              case Expr(x) => Expr(x.useRawName)

    loop[M]

  transparent inline private def arity(e: MadeElem): SlotArity =
    inline if e.hasAnnotation[mrpc.annotation.multi] then SlotArity.Multi
    else inline if e.hasAnnotation[mrpc.annotation.optional] then SlotArity.Optional
    else SlotArity.Single

  inline private def fillParam(e: MadeElem)(using Context) =
    inline if e.hasAnnotation[mrpc.annotation.composite] then composite[e.Type]
    else inline if e.hasAnnotation[mrpc.annotation.reifyName] then reifyName(e.getUserRawName)
    else inline if e.hasAnnotation[mrpc.annotation.reifyAnnot] then reifyAnnot[e.Type](arity(e))
    else inline if e.hasAnnotation[mrpc.annotation.rpcMethodMetadata] then
      inline ctx match
        case ctx: Context.Trait => rpcMethodMetadata[e.Type, e.Label](arity(e))(using ctx)
    else inline if e.hasAnnotation[mrpc.annotation.rpcParamMetadata] then
      inline ctx match
        case ctx: Context.Method => rpcParamMetadata[e.Type, e.Label](arity(e))(using ctx)
    else inline if e.hasAnnotation[mrpc.annotation.infer] then compiletime.summonInline[e.Type]
    else
      compiletime.error(
        "metadata param '" + compiletime.constValue[e.Label] + "' has no recognized steering annotation " +
          "(@composite/@reifyName/@reifyAnnot/@infer/@rpcMethodMetadata/@rpcParamMetadata)",
      )

  /**
   * The collection-arity marker on a metadata param. `@multi` -> a collection slot; `@optional` ->
   * an `Option` slot; absent (or `@single`) -> exactly-one. Mirrors commons `single`/`optional`/`multi`.
   */
  private sealed trait SlotArity
  private object SlotArity:
    sealed trait Single extends SlotArity
    object Single extends Single
    sealed trait Optional extends SlotArity
    object Optional extends Optional
    sealed trait Multi extends SlotArity
    object Multi extends Multi

  /**
   * `@composite`: the param's type C is a class with a public primary ctor; recurse `buildValue(C, ctx)`
   * threading the SAME real-symbol context, so C's `@reifyName` reads the enclosing real symbol (the op
   * or param), NOT C's field names. Emits `new C(<filled params>)`.
   */
  inline private def composite[M](using Context): M =
    buildValue[M]

  /** `@reifyName`: source label or, when `useRaw`, the resolved rpcName for the current context. */
  inline private def reifyName(inline useRaw: Boolean)(using Context): String = inline ctx match
    case ctx: Context.Method =>
      inline if useRaw then compiletime.constValue[ctx.Name]
      else compiletime.constValue[ctx.op.Label]
    case ctx: Context.Param =>
      compiletime.constValue[ctx.underlying.Label]
    case _: Context.Trait => compiletime.error("@reifyName is not valid at the trait level (no single name)")

  /**
   * `@reifyAnnot` (arity-sensitive): finds the param's annotation type `A` in the current context's
   * captured `Metadata` via made's `getAllAnnotations`. Slot shape by arity:
   *   - `@single` (or bare slot `A`)   -> the single instance; aborts if absent.
   *   - `@optional` / slot `Option[A]` -> `Some(a)`/`None`.
   *   - `@multi` / slot `List[A]`      -> all matching annotations on the symbol.
   *
   * Pulled out as its own `inline` def rather than nested inside `reifyAnnot` because nested `inline`
   * defs aren't supported, and `ctx.op`/`ctx.underlying`'s captured `Metadata` is only visible to
   * `getAllAnnotations` while this match is itself `inline` in a context where `ctx` hasn't already
   * been widened to the bare `Context` supertype.
   */
  inline private def allTerms[A](using Context): List[A] = inline ctx match
    case ctx: Context.Method => getAllAnnotations(ctx.op)(using containsOnly.refl)[A & Annotation]
    case ctx: Context.Param => getAllAnnotations(ctx.underlying)(using containsOnly.refl)[A & Annotation]
    case _: Context.Trait => compiletime.error("@reifyAnnot is not valid at the trait level")

  inline private def reifyAnnot[Param](arity: SlotArity)(using Context): Any =
    inline (arity, compiletime.erasedValue[Param]) match
      case (_: SlotArity.Multi, _: List[a]) => allTerms[a]
      case (_: SlotArity.Multi, _) => allTerms[Param]
      case (_: SlotArity.Optional, _: Option[a]) => allTerms[a].headOption
      case (_, _: Option[a]) => allTerms[a].headOption
      case (_, _) =>
        inline allTerms[Param] match
          case term :: _ => term
          case Nil => compiletime.error("@single @reifyAnnot: no annotation present on the RPC element")

  /**
   * `@rpcMethodMetadata`: projects over the trait's ops. Arity-shaped:
   *   - `@multi List[E]`             -> one element per op, collected into a `List`.
   *   - `@multi Map[String, E]`      -> one element per op, keyed by resolved rpcName.
   *   - `@optional Option[E]`        -> the single op if exactly one, else `None`.
   *   - `@single E` (or default)     -> the single op; aborts on 0 or >1.
   * Each element recurses `buildValue` in a per-op [[Context.Method]].
   */

  inline private def buildAllMethodElems[e, Names <: Tuple](
    ops: Tuple,
  )(using Names containsOnly String,
  )(using ops.type containsOnly DoneOperation,
  ): Tuple =
    inline (ops, compiletime.erasedValue[Names]) match
      case (_: EmptyTuple, _: EmptyTuple) => EmptyTuple
      case (_: (opHead *: opTail), _: (nameHead *: nameTail)) =>
        val opHead = ops.head.asInstanceOf[opHead & DoneOperation]
        val opTail = ops.tail.asInstanceOf[opTail & Tuple.Tail[ops.type]]
        realCons(
          buildElem[e, opHead.Type](using Context.Method[nameHead & String, opHead & DoneOperation](opHead)),
          buildAllMethodElems[e, Tuple.Tail[Names]](opTail),
        )

  inline private def buildAllParamElems[e](params: Tuple)(using params.type containsOnly InputElem): Tuple =
    inline params match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        val head = params.head.asInstanceOf[head & InputElem]
        val tail = params.tail.asInstanceOf[tail & Tuple.Tail[params.type]]

        realCons(buildElem[e, head.Type](using Context.Param(head)), buildAllParamElems[e](tail))

  inline private def buildElem[elem <: AnyKind, T](using ctx: Context) = ${ buildElemImpl[elem, T]('ctx) }

  private def buildElemImpl[elem <: AnyKind: Type, T: Type](
    ctx: Expr[Context],
  )(using Quotes,
  ): Expr[?] = Type.of[elem] match
    case '[type f[_] <: Any; f] =>
      '{ buildValue[f[T]](using compiletime.summonInline[Made.Of[f[T]]], $ctx) }
    case '[type e <: Any; e] =>
      '{ buildValue[e](using compiletime.summonInline[Made.Of[e]], $ctx) }

  inline private def rpcMethodMetadata[P, Name <: String](arity: SlotArity)(using ctx: Context.Trait): Any =
    inline arity match
      case _: SlotArity.Multi =>
        inline compiletime.erasedValue[P] match
          case _: Map[String, e] =>
            NamedTuple
              .build[ctx.Names]()(buildAllMethodElems[e, ctx.Names](ctx.operations))
              .toList
              .asInstanceOf[List[(String, ?)]]
              .toMap
          case _: List[e] =>
            buildAllMethodElems[e, ctx.Names](ctx.operations).toList
      case _: SlotArity.Optional =>
        inline compiletime.erasedValue[P] match
          case _: Option[e] =>
            inline ctx.operations match
              case _: EmptyTuple => None
              case (it: DoneOperation) :: Nil => Some(buildElem[e, it.Type])
              case _ =>
                compiletime.error(
                  "@rpcMethodMetadata @optional slot '" + compiletime.constValue[Name] + "' matched " +
                    ctx.operations.size + " elements; expected 0 or 1",
                )
      case _: SlotArity.Single =>
        inline ctx.operations match
          case (it: DoneOperation) *: EmptyTuple => buildElem[P, it.Type]
          case other =>
            compiletime.error(
              "@rpcMethodMetadata @single slot '" + compiletime.constValue[Name] +
                "' requires exactly one match; got " + other.size,
            )

  /**
   * `@rpcParamMetadata`: projects over the op's `inputElems` (declaration order). Same arity shaping as
   * [[rpcMethodMetadata]]; `Map` slots are keyed by paramName.
   */
  inline private def rpcParamMetadata[P, Name <: String](arity: SlotArity)(using Context.Method): P =
    inline arity match
      case _: SlotArity.Multi =>
        inline compiletime.erasedValue[P] match
          case _: Map[String, e] =>
            NamedTuple
              .build[Tuple.Map[ctx.op.InputElems, InputElem.ExtractLabel]]()(buildAllParamElems[e](ctx.op.inputElems))
              .toList
              .asInstanceOf[List[(String, ?)]]
              .toMap
              .asInstanceOf[P]
          case _: List[e] =>
            buildAllParamElems[e](ctx.op.inputElems).toList.asInstanceOf[P]
      case _: SlotArity.Optional =>
        inline compiletime.erasedValue[P] match
          case _: Option[e] =>
            inline ctx.op.inputElems match
              case _: EmptyTuple => None.asInstanceOf[P]
              case (it: InputElem) *: EmptyTuple => Some(buildElem[e, it.Type](using Context.Param(it))).asInstanceOf[P]
              case _ =>
                compiletime.error(
                  "@rpcParamMetadata @optional slot '" + compiletime.constValue[Name] + "' matched " +
                    ctx.op.inputElems.size + " elements; expected 0 or 1",
                )
      case _: SlotArity.Single =>
        inline ctx.op.inputElems match
          case (it: InputElem) *: EmptyTuple => buildElem[P, it.Type](using Context.Param(it)).asInstanceOf[P]
          case other =>
            compiletime.error(
              "@rpcParamMetadata @single slot '" + compiletime.constValue[Name] + "' requires exactly one match; got " +
                other.size,
            )
