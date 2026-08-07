package mrpc
package derive

import made.*

import scala.concurrent.Future
import scala.quoted.{Expr, Quotes, Type}

/**
 * Type-level tag for which `RawRpc` dispatch method (`fire`/`call`/`get`) an operation routes to.
 * `Call`/`Get` carry their payload (the `Future` result type / the sub-RPC type) as a type member, so
 * it survives as part of an [[OpPlan]]'s own type — classified via `inline compiletime.erasedValue`
 * matches, no separate value-level enum to keep in sync with it.
 */
private[derive] sealed class ArityTag
private[derive] object ArityTag:
  sealed class Fire extends ArityTag
  sealed class Call[Result] extends ArityTag
  sealed class Get[Sub] extends ArityTag

/** Type-level encode-vs-verbatim tag, classified the same way as [[ArityTag]]. */
private[derive] sealed trait EncodingTag
private[derive] object EncodingTag:
  sealed trait Encoded extends EncodingTag
  sealed trait Verbatim extends EncodingTag

private[derive] sealed trait ParamPlan:
  type Label <: String
  type ParamType
  type Encoding <: EncodingTag

object ParamPlan:
  private val reusable = new ParamPlan {}

  inline private def isRawCarrier[T]: Boolean = ${ isRawCarrierImpl[T] }

  private def isRawCarrierImpl[T: Type](using quotes: Quotes): Expr[Boolean] =
    import quotes.reflect.*
    // An abstract type member / type parameter (no concrete dealias) is the only thing that could be
    // the engine's `Raw`. Concrete leaf types (Int, String, User, ...) are always encoded. Whether a
    // type is abstract vs. concrete isn't a named member to pattern-match on, so this stays reflect-API.
    Expr(TypeRepr.of[T].typeSymbol.isAbstractType)

  transparent inline private def encodingOf(ie: InputElem): TypeProxy { type Underlying <: EncodingTag } =
    inline if ie.hasAnnotation[mrpc.annotation.verbatim] && isRawCarrier[ie.Type]
    then TypeProxy[EncodingTag.Verbatim]
    else TypeProxy[EncodingTag.Encoded]

  inline def apply(ie: InputElem, encodingTag: TypeProxy)
    : ParamPlan { type Label = ie.Label; type ParamType = ie.Type; type Encoding = encodingTag.Underlying } =
    reusable.asInstanceOf[
      ParamPlan {
        type Label = ie.Label
        type ParamType = ie.Type
        type Encoding = encodingTag.Underlying
      },
    ]

  transparent inline def materialize(ie: InputElem): ParamPlan =
    apply(ie, encodingOf(ie))

/**
 * One operation's classification, at the type level: arity, resolved rpcName, per-parameter encode
 * plan, and the underlying `DoneOperation` this plan describes. [[Plans]] collects a `Tuple` of these,
 * one per `Done.Operations` entry, in the same order, so a plan's position in that tuple IS its
 * `Done.Operations` index. There is no runtime-comparable projection of an `OpPlan` — only its static
 * type is ever read, via inline/quote-pattern matches.
 */
private[derive] sealed trait OpPlan:
  type Label <: String
  type RpcName <: String
  type ArityInfo <: ArityTag
  type Params <: Tuple
  type ResultEncoding <: EncodingTag
  type OpType <: DoneOperation

  type Args <: Tuple

object OpPlan:

  private val reusable = new OpPlan {}

  transparent inline private def arityOf[Output]: TypeProxy = inline compiletime.erasedValue[Output] match
    case _: Unit => TypeProxy[ArityTag.Fire]
    case _: Future[x] => TypeProxy[ArityTag.Call[x]]
    case _ => TypeProxy[ArityTag.Get[Output]]

  transparent inline def materialize[Name <: String](op: DoneOperation): OpPlan =
    build[op.type, Name, op.Label](arityOf[op.OutputType], buildParams(op.inputElems))

  transparent inline def build[Op <: DoneOperation, Name <: String, label <: String](
    arity: TypeProxy,
    params: Tuple,
  ): OpPlan = reusable.asInstanceOf[
    OpPlan {
      type Label = label
      type RpcName = Name
      type ArityInfo = arity.Underlying
      type Params = params.type
      type ResultEncoding = EncodingTag.Encoded
      type OpType = Op
      type Args = Tuple.Map[
        params.type,
        [X] =>> X match
          case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
      ]
    },
  ]

  transparent inline def buildParams(elems: Tuple)(using elems.type containsOnly InputElem): Tuple =
    inline elems match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: tail4) =>
        realCons(
          ParamPlan.materialize(elems.head.asInstanceOf[h & InputElem]),
          buildParams(elems.tail.asInstanceOf[tail4 & Tuple.Tail[elems.type]]),
        )

object Plans:
  private type Proxy0 = TypeProxy { type Underlying <: Tuple }
  opaque type Proxy[T] <: Proxy0 = Proxy0

  transparent inline def apply[T](inline plans: Tuple): Proxy[T] =
    ${ applyImpl[T]('plans) }

  def applyImpl[T](plans: Expr[Tuple])(using quotes: Quotes): Expr[Proxy[T]] =
    import quotes.reflect.*
    plans.asTerm.tpe.widen.asType match
      case '[type tup <: Tuple; tup] =>
        '{ TypeProxy[tup] }
  transparent inline def materialize[T: {Done.Of as done}](names: RpcNames.Proxy[T]): Proxy[T] =
    Plans(buildAll[names.Underlying](done.operations))

  transparent inline def buildAll[Names <: Tuple](
    operations: Tuple,
  )(using
    Names containsOnly String,
    operations.type containsOnly DoneOperation,
  ): Tuple = inline compiletime.erasedValue[Names] match
    case _: EmptyTuple => EmptyTuple
    case _: (name *: nextNames) =>
      inline operations match
        case _: EmptyTuple => EmptyTuple
        case _: (h *: nextOps) =>
          realCons(
            OpPlan.materialize[name & String](operations.head.asInstanceOf[h & DoneOperation]),
            buildAll[nextNames & Tuple.Tail[Names]](operations.tail.asInstanceOf[nextOps & Tuple.Tail[operations.type]]),
          )
