package mrpc.derive

import made.{Done, DoneOperation, InputElem, Meta}

import scala.concurrent.Future
import scala.quoted.{Expr, Quotes, Type}

/**
 * Type-level tag for which `RawRpc` dispatch method (`fire`/`call`/`get`) an operation routes to.
 * `Call`/`Get` carry their payload (the `Future` result type / the sub-RPC type) as a type member, so
 * it survives as part of an [[OpPlan]]'s own type. The matcher and the two materialize macros all
 * classify arity by quote-pattern-matching this type directly (`case '[ArityTag.Fire] => ...` /
 * `case '[ArityTag.CallOf[r]] => ...`) — there is no separate value-level enum to keep in sync with it.
 */
private[derive] sealed trait ArityTag
private[derive] object ArityTag:
  sealed trait Fire extends ArityTag
  sealed trait Call extends ArityTag:
    type Result
  sealed trait Get extends ArityTag:
    type Sub

  type CallOf[R] = Call { type Result = R }
  type GetOf[S] = Get { type Sub = S }

/**
 * Type-level encode-vs-verbatim tag, classified the same way as [[ArityTag]] — via quote-pattern
 * matching directly on the type, no value-level enum counterpart.
 */
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
 * plan, and the underlying `DoneOperation` this plan describes. [[Matcher.plans]] builds a `List` of
 * these — one per `Done.Operations` entry, in the same order, so a plan's position in that list IS
 * its `Done.Operations` index. Mirrors how made's `Done` models `T` as a `Tuple` of `DoneOperation`s.
 * Read back via local quote-pattern reads on the plan's `Type[?]` (`Matcher`, `AsRawDerivation`, `AsRealDerivation` each
 * keep their own), or directly by test code via `Matcher.planFor`/`Matcher.plansFor` + `summon[... =:=
 * ...]` (see `MatchingSuite`/`RpcNameSuite`) — there is no runtime-comparable projection of an `OpPlan`.
 */
private[derive] sealed trait OpPlan:
  type Label <: String
  type RpcName <: String
  type ArityInfo <: ArityTag
  type Params <: Tuple
  type ResultEncoding <: EncodingTag
  type OpType <: DoneOperation

  final type Args  = Tuple.Map[
    Params,
    [X] =>> X match
      case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
  ]

object OpPlan:

  type Of[Op <: DoneOperation, Name <: String] = OpPlan {
    type OpType = Op
    type RpcName = Name
  }
  transparent inline given [Op <: DoneOperation, Name <: String] => OpPlan.Of[Op, Name] = ${ impl[Op, Name] }
  private def impl[Op: Type, Name <: String: Type](using quotes: Quotes): Expr[OpPlan.Of[Op, Name]] =
    import quotes.reflect.*
    val arityType = Type.of[Op] match
      case '[{ type OutputType = Unit }] => Type.of[ArityTag.Fire]
      case '[{ type OutputType = Future[x] }] => Type.of[ArityTag.CallOf[x]]
      case '[{ type OutputType = other }] => Type.of[ArityTag.GetOf[other]]
    val paramsType: Type[? <: Tuple] = TupleTraverse.foldTuple(Type.of[Op] match
      case '[type elems <: Tuple; DoneOperation { type InputElems = elems }] =>
        TupleTraverse
          .traverseTuple[elems, InputElem]
          .map { case '[type m <: Tuple; InputElem { type Label = l; type Type = t; type Metadata = m }] =>
            def isVerbatim[T: Type] = TypeRepr.of[T] match
              case AnnotatedType(_, annot) => annot.tpe <:< TypeRepr.of[mrpc.annotation.verbatim]
              case _ => false
            def isRawCarrier[T: Type]: Boolean = TypeRepr.of[T].typeSymbol.isAbstractType

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
          new OpPlan:
            type Label = l
            type RpcName = Name
            type ArityInfo = a
            type Params = ps
            type ResultEncoding = EncodingTag.Encoded
            type OpType = Op
        }.asExprOf[OpPlan.Of[Op, Name]]
      case _ => report.errorAndAbort(s"could not build OpPlan for operation ${Type.show[Op]}")

opaque type Plans[T] <: Tuple = Tuple

object Plans:
  transparent inline given [T: {Done.Of as done, RpcNames as names}] => Plans[T] =
    compiletime.summonAll[
      Tuple.Map[
        Tuple.Zip[done.Operations, names.Names],
        [x] =>> x match
          case (op, n) => OpPlan.Of[op, n],
      ],
    ]
