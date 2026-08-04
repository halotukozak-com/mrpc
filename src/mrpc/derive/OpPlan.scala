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

  transparent inline def materialize[Op <: DoneOperation, Name <: String]: OpPlan = ${ materializeImpl[Op, Name] }

  /**
   * Classifies a single operation (its resolved rpcName + its `DoneOperation` type) into a fully
   * refined [[OpPlan]] type: arity (from `OutputType`), a per-parameter encode-vs-verbatim plan, and
   * the underlying `OpType`/`RpcName`. The ONLY place classification happens — [[Plans]] (which collects
   * one per `T`, once) is its only caller; every other consumer reads already-classified [[OpPlan]]s
   * back off [[Plans]] instead of re-deriving them.
   *
   * MUST be a macro, not a `transparent inline given` summoned per position via `Tuple.Map`: the
   * richer type returned here (`ArityInfo`/`Params`/... set, not just `OpType`/`RpcName`) does not
   * propagate through `compiletime.summonAll` — the tuple's static element type there is fixed to
   * whatever alias the `Tuple.Map` lambda names, regardless of what the `given` actually infers.
   */
  private[derive] def materializeImpl[Op <: DoneOperation: Type, Name <: String: Type](using quotes: Quotes)
    : Expr[OpPlan] =
    import quotes.reflect.*
    val arityType = Type.of[Op] match
      case '[{ type OutputType = Unit }] => Type.of[ArityTag.Fire]
      case '[{ type OutputType = Future[x] }] => Type.of[ArityTag.Call[x]]
      case '[{ type OutputType = other }] => Type.of[ArityTag.Get[other]]
    val paramsType: Type[? <: Tuple] = TupleTraverse.foldTuple(Type.of[Op] match
      case '[type elems <: Tuple; DoneOperation { type InputElems = elems }] =>
        TupleTraverse
          .traverseTuple[elems, InputElem]
          .map { case '[type m <: Tuple; InputElem { type Label = l; type Type = t; type Metadata = m }] =>
            def isVerbatim[T: Type] = TypeRepr.of[T] match
              case AnnotatedType(_, annot) => annot.tpe <:< TypeRepr.of[mrpc.annotation.verbatim]
              case _ => false

            val encodingType =
              if TupleTraverse.traverseTuple[m, Meta].exists(isVerbatim(using _)) && OpReflect.isRawCarrier[t]
              then Type.of[EncodingTag.Verbatim]
              else Type.of[EncodingTag.Encoded]
            encodingType match
              case '[type e <: EncodingTag; e] =>
                Type.of[ParamPlan { type Label = l; type ParamType = t; type Encoding = e }]
          })

    // Under the fixed-RawRpc model the result is always encoded via the summoned leaf bridge unless
    // it is itself Raw; @verbatim on a non-Raw result has no identity to land on. Result-verbatim is
    // therefore only reachable through the same Raw-type check params use; v1 fixtures encode results.
    (Type.of[Op], arityType, paramsType) match
      case (
            '[type l <: String; { type Label = l }],
            '[type a <: ArityTag; a],
            '[type ps <: Tuple; ps],
          ) =>
        '{
          reusable.asInstanceOf[
            OpPlan {
              type Label = l
              type RpcName = Name
              type ArityInfo = a
              type Params = ps
              type ResultEncoding = EncodingTag.Encoded
              type OpType = Op
            },
          ]
        }
      case _ => report.errorAndAbort(s"could not build OpPlan for operation ${Type.show[Op]}")

/**
 * Every operation of `T`, classified once — the single authority [[AsRawDerivation]] and
 * [[AsRealDerivation]] both read from, instead of each re-deriving [[OpPlan]]s of their own.
 * `Underlying` is a tuple of fully refined [[OpPlan]] types, one per `Done.Operations` entry, in
 * order — never dereferenced as a value, same convention as [[OpPlan]] itself.
 */
sealed trait Plans[T]:
  type Underlying <: Tuple
  given Underlying containsOnly OpPlan = containsOnly.refl

object Plans:
  private val reusable = new Plans[Any] {}

  private def apply[T](plans: Tuple): Plans[T] { type Underlying = plans.type } = reusable.asInstanceOf[
    Plans[T] {
      type Underlying = plans.type
    },
  ]

  transparent inline def materialize[T: {Done.Of as done, RpcNames as names}]: Plans[T] =
    Plans(buildAll[Tuple.Zip[names.Underlying, done.Operations]])

  transparent inline private def buildAll[Acc <: Tuple](
    using Acc containsOnly (String, DoneOperation),
  ): Tuple = inline compiletime.erasedValue[Acc] match
    case _: EmptyTuple => EmptyTuple
    case _: ((name, op) *: next) =>
      buildAll[next & Tuple.Tail[Acc]].realCons(OpPlan.materialize[op & DoneOperation, name & String])
