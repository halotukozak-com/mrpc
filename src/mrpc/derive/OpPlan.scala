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

/**
 * One parameter's plan, at the type level: its label, declared type, and encode-vs-verbatim tag.
 * [[OpPlan.Params]] is a `Tuple` of these — the parameter-level counterpart of [[OpPlan]] itself.
 */
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

  transparent inline def materialize[Elem <: InputElem]: ParamPlan = inline compiletime.erasedValue[Elem] match
    case ie =>
      val encodingType =
        inline if ie.hasAnnotation[mrpc.annotation.verbatim] && isRawCarrier[ie.Type]
        then TypeProxy[EncodingTag.Verbatim]
        else TypeProxy[EncodingTag.Encoded]

      reusable.asInstanceOf[
        ParamPlan {
          type Label = ie.Label;
          type ParamType = ie.Type;
          type Encoding = encodingType.Underlying
        },
      ]

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

  final type Args = Tuple.Map[
    Params,
    [X] =>> X match
      case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
  ]

object OpPlan:

  private val reusable = new OpPlan {}

  /**
   * [[OpPlan.Args]], off a still-abstract `Plan <: OpPlan` — `Plan#Args` (general type projection on a
   * non-singleton prefix) is disallowed, and `Args` itself is a concrete alias (not a bare abstract
   * member), so a match type can't refine it directly either; this recomputes it from `Params` (which
   * IS abstract) instead. Only needed in ordinary (non-macro) declarations like [[Handler]]'s trait
   * hierarchy; macro code just quote-pattern-matches `Type[Plan]` directly.
   */
  type ArgsOf[Plan <: OpPlan] = Plan match
    case ([p <: Tuple] =>> OpPlan { type Params = p })[params] =>
      Tuple.Map[
        params,
        [X] =>> X match
          case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
      ]

  transparent inline private def arityOf[Output]: TypeProxy = inline compiletime.erasedValue[Output] match
    case _: Unit => TypeProxy[ArityTag.Fire]
    case _: Future[x] => TypeProxy[ArityTag.Call[x]]
    case _ => TypeProxy[ArityTag.Get[Output]]

  transparent inline def materialize[Op <: DoneOperation, Name <: String]: OpPlan =
    inline compiletime.erasedValue[Op] match
      case op =>
        build[Op, Name, op.Label](arityOf[op.OutputType], buildParams[op.InputElems])
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
    },
  ]

  transparent inline private def buildParams[Acc <: Tuple](using Acc containsOnly InputElem): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: tail) =>
        buildParams[tail & Tuple.Tail[Acc]].realCons(ParamPlan.materialize[h & InputElem])

object Plans:
  private type Proxy0 = TypeProxy { type Underlying <: Tuple }
  opaque type Proxy[T] <: Proxy0 = Proxy0

  given [T, Under <: Tuple, P <: Proxy[T] { type Underlying = Under }] => (Under containsOnly OpPlan) =
    containsOnly.refl

  private def apply[T](plans: Tuple): Proxy[T] { type Underlying = plans.type } =
    TypeProxy.apply[plans.type]

  transparent inline def materialize[T: {Done.Of as done}](names: RpcNames.Proxy[T]): Proxy[T] =
    Plans(buildAll[Tuple.Zip[names.Underlying, done.Operations]])

  transparent inline private def buildAll[Acc <: Tuple](
    using Acc containsOnly (String, DoneOperation),
  ): Tuple = inline compiletime.erasedValue[Acc] match
    case _: EmptyTuple => EmptyTuple
    case _: ((name, op) *: next) =>
      buildAll[next & Tuple.Tail[Acc]].realCons(OpPlan.materialize[op & DoneOperation, name & String])
