package halotukozak.mrpc
package derive

import commons.{containsOnly, realCons, Of}
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
  object Fire extends ArityTag
  final class Call[Result] extends ArityTag
  final class Get[Sub] extends ArityTag

/** Type-level encode-vs-verbatim tag, classified the same way as [[ArityTag]]. */
private[derive] enum EncodingTag:
  case Encoded, Verbatim

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

  transparent inline private def encodingOf(ie: InputElem): EncodingTag =
    inline if ie.hasAnnotation[halotukozak.mrpc.annotation.verbatim] && isRawCarrier[ie.Type]
    then EncodingTag.Verbatim
    else EncodingTag.Encoded

  inline def apply(ie: InputElem, encodingTag: EncodingTag)
    : ParamPlan { type Label = ie.Label; type ParamType = ie.Type; type Encoding = encodingTag.type } =
    reusable.asInstanceOf[
      ParamPlan {
        type Label = ie.Label
        type ParamType = ie.Type
        type Encoding = encodingTag.type
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
  type ParamLists <: Tuple

object OpPlan:

  private val reusable = new OpPlan {}

  transparent inline private def arityOf[Output]: ArityTag = inline compiletime.erasedValue[Output] match
    case _: Unit => ArityTag.Fire
    case _: Future[x] => ArityTag.Call[x]
    case _ => ArityTag.Get[Output]

  transparent inline def materialize[Name <: String](op: DoneOperation): OpPlan =
    build[op.type, Name, op.Label, op.ParamLists](arityOf[op.OutputType], buildParams(op.inputElems))

  transparent inline def build[Op <: DoneOperation, Name <: String, label <: String, Lists <: Tuple](
    arity: ArityTag,
    params: Tuple,
  ): OpPlan = reusable.asInstanceOf[
    OpPlan {
      type Label = label
      type RpcName = Name
      type ArityInfo = arity.type
      type Params = params.type
      type ResultEncoding = EncodingTag.Encoded.type
      type OpType = Op
      type Args = Tuple.Map[
        params.type,
        [X] =>> X match
          case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
      ]
      type ParamLists = Lists
    },
  ]

  transparent inline private def buildParams(elems: Tuple)(using elems.type containsOnly InputElem): Tuple =
    inline elems match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        realCons(
          ParamPlan.materialize(elems.head.asInstanceOf[head & InputElem]),
          buildParams(elems.tail.asInstanceOf[tail & Tuple.Tail[elems.type]]),
        )

object Plans:
  transparent inline def materialize[T: {Done.Of as done}](names: Tuple)(using names.type containsOnly String): Tuple =
    buildAll[names.type](done.operations)

  transparent inline private def buildAll[Names <: Tuple: Of[String]](
    operations: Tuple,
  )(using operations.type containsOnly DoneOperation,
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
