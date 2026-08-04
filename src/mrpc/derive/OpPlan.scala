package mrpc.derive

import made.{containsOnly, Done, DoneOperation, InputElem, Meta}

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
 * plan, and the underlying `DoneOperation` this plan describes. [[Plans]] collects a `Tuple` of these
 * — one per `Done.Operations` entry, in the same order, so a plan's position in that tuple IS
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

  /** [[OpPlan.Args]], off a still-abstract `Op <: OpPlan` — `Op#Args` (general type projection on a
   * non-singleton prefix) is disallowed, and `Args` itself is a concrete alias (not a bare abstract
   * member), so a match type can't refine it directly either; this recomputes it from `Params` (which
   * IS abstract) via the same type-lambda-application idiom [[RpcNames]]/[[Matcher]] use to extract a
   * member off an abstract type parameter. Only needed in ordinary (non-macro) declarations like
   * [[Handler]]'s trait hierarchy; macro code just quote-pattern-matches `Type[Op]` directly.
   */
  type ArgsOf[Op <: OpPlan] = Op match
    case ([p <: Tuple] =>> OpPlan { type Params = p })[params] =>
      Tuple.Map[
        params,
        [X] =>> X match
          case ([p0] =>> ParamPlan { type ParamType = p0 })[p] => p,
      ]

  /** Classifies a single operation (its resolved rpcName + its `DoneOperation` type) into a fully
   * refined [[OpPlan]] type: arity (from `OutputType`), a per-parameter encode-vs-verbatim plan, and
   * the underlying `OpType`/`RpcName`. The ONLY place classification happens — [[Plans]] (which collects
   * one per `T`, once) is its only caller; [[Matcher]] and every other consumer read already-classified
   * [[OpPlan]]s back off [[Plans]] instead of re-deriving them.
   *
   * MUST be a macro (not an ordinary `transparent inline given`, `Tuple.Map`-summoned per position):
   * the richer type it returns here (`ArityInfo`/`Params`/... set, not just `OpType`/`RpcName`) does NOT
   * propagate through a plain `compiletime.summonAll[Tuple.Map[..., SomeAlias[op, n]]]` — the tuple's
   * static element type is fixed to whatever alias the `Tuple.Map` lambda names, regardless of what a
   * `transparent inline given` infers when summoned into that position. [[Plans]] therefore builds its
   * `All` tuple type directly from this macro-level `Type[? <: OpPlan]`, not from a per-op `given`.
   */
  private[derive] def classify(opType: Type[? <: DoneOperation], rpcName: Type[? <: String])(using quotes: Quotes)
    : Type[? <: OpPlan] =
    import quotes.reflect.*
    val arityType = opType match
      case '[{ type OutputType = Unit }] => Type.of[ArityTag.Fire]
      case '[{ type OutputType = Future[x] }] => Type.of[ArityTag.CallOf[x]]
      case '[{ type OutputType = other }] => Type.of[ArityTag.GetOf[other]]
    val paramsType: Type[? <: Tuple] = TupleTraverse.foldTuple(opType match
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
    (opType, rpcName, arityType, paramsType, opType) match
      case (
            '[type l <: String; { type Label = l }],
            '[type n <: String; n],
            '[type a <: ArityTag; a],
            '[type ps <: Tuple; ps],
            '[o],
          ) =>
        Type.of[
          OpPlan {
            type Label = l
            type RpcName = n
            type ArityInfo = a
            type Params = ps
            type ResultEncoding = EncodingTag.Encoded
            type OpType = o
          },
        ]
      case _ => report.errorAndAbort(s"could not build OpPlan for operation ${TypeRepr.of(using opType).show}")

/**
 * Every operation of `T`, classified once — the single authority [[Matcher]] and the client-proxy
 * materialization ([[AsRealDerivation]] / [[Handler]]) both read from, instead of each re-deriving
 * [[OpPlan]]s of their own. `All` is a tuple of fully refined [[OpPlan]] types (one per
 * `Done.Operations` entry, in order); `all` is the same tuple as a runtime value (never dereferenced,
 * same convention as [[OpPlan]] itself — only its STATIC type is ever read).
 */
sealed trait Plans[T]:
  type Underlying <: Tuple
  given Underlying containsOnly OpPlan = containsOnly.refl

object Plans:
  transparent inline given [T: {Done.Of as done, RpcNames as names}] => Plans[T] = ${ derive[T, names.Names, done.Operations]('done) }

  /**
   * Builds `Plans[T]`'s `All` from [[OpPlan.classify]], one op at a time (in `Done.Operations` order) —
   * a macro, not a `Tuple.Map`-over-a-per-op-`given`, precisely because `classify`'s richer per-op type
   * (see its doc) only exists at this macro level; folding `Type[? <: OpPlan]`s with
   * [[TupleTraverse.foldTuple]] is what actually carries it into `All`.
   */
  private def derive[T: Type, Names <: Tuple: Type, Operations <: Tuple: Type](done: Expr[Done.Of[T]])(using Quotes): Expr[Plans[T]] =
    val ops = TupleTraverse.traverseTuple[Operations, DoneOperation]
    val resolvedNames = TupleTraverse.traverseTuple[Names, String]
    val opPlanTypes = ops.zip(resolvedNames).map((op, name) => OpPlan.classify(op, name))
    TupleTraverse.foldTuple(opPlanTypes) match
      case '[type all <: Tuple; all] =>
        '{
          (new Plans[T]:
            type Underlying = all
          ): Plans[T] { type Underlying = all }
        }
