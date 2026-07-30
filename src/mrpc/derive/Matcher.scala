package mrpc.derive

import made.{Done, DoneOperation, InputElem}
import mrpc.derive.OpReflect.paramOf

import scala.concurrent.Future
import scala.quoted.*

/**
 * Pure compile-time classification of each `DoneOperation` into an [[OpPlan]]: arity (from the
 * output type), the resolved rpcName, and a per-parameter encode-vs-verbatim plan.
 *
 * This is deliberately a per-operation CLASSIFICATION, not an N×M raw/real matching search. mrpc's
 * `RawRpc[Raw]` is the fixed three-method trait (`fire`/`call`/`get`), so there are no user-declared
 * raw methods to match against: every real op routes to its arity's single raw method, carrying its
 * rpcName inside `RawInvocation.rpcName`. That reframing is what keeps this component small.
 *
 * Tag annotations (`@tagged`/`@methodTag`/`@paramTag`) are intentionally NOT read here: with one raw
 * method per arity there is no tag-selection branch to drive. This object is the seam where a future
 * generic-raw-method matcher would plug tag logic in.
 *
 * Widened to `private[mrpc]` because it is the shared introspection reused by BOTH the engine
 * (matching/dispatch) and metadata materialization — one Done-walk path, no fork.
 */
private[mrpc] object Matcher:

  /**
   * Classifies every operation of `T` into an [[OpPlan]] and folds the results into a `Plans <:
   * Tuple` type — one [[OpPlan]] per `Done.Operations` entry, in the same order — mirroring how
   * `Done` itself models `T` as a `Tuple` of `DoneOperation`s. rpcName resolution (incl. overload
   * disambiguation + duplicate detection) is delegated to [[RpcName.computeAll]]. The `Done` mirror
   * is passed in (summoned once at the entry point via [[summonDone]]), not re-summoned here.
   */
  def planAll[T: Type](done: Expr[Done.Of[T]], names: Expr[RpcNames[T]])(using Quotes): Type[? <: Tuple] =
    val ops = done match
      case '{ type operations <: Tuple; $_ : { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]
    // Names come from the single authority `RpcNames[T]` (type-level `Names`, read back by `namesOf`).
    // `namesOf` falls back to a direct `computeAll` when the mirror can't be summoned, so the
    // duplicate-name abort still surfaces verbatim (see CompileErrorSuite).
    val resolvedNames = RpcNames.namesOf[T](names)
    val planTypes: List[Type[?]] = ops.zip(resolvedNames).map(planOne)
    TupleTraverse.foldTuple(planTypes)

  /**
   * The consumer-facing entry point: [[planAll]]'s `Plans` tuple, traversed back into the
   * `List[Type[?]]` of individual [[OpPlan]]s the server adapter and client proxy macros actually
   * iterate over — each queried on demand via local quote-pattern reads on the plan's `Type[?]`, same
   * as [[operationTypes]]'s list of `DoneOperation` types is queried via [[OpReflect]].
   */
  def plans[T: Type](done: Expr[Done.Of[T]], names: Expr[RpcNames[T]])(using Quotes): List[Type[? <: OpPlan]] =
    planAll[T](done, names) match
      case '[type ps <: Tuple; ps] => TupleTraverse.traverseTuple[ps, OpPlan]

  /**
   * Exposes a SINGLE operation's [[OpPlan]] type directly to the CALLER's type checker —
   * `transparent inline`, the same technique `made.Done.derived` uses to make its refined type
   * visible outside the macro. Lets ordinary, non-macro code (tests) assert compile-time facts about
   * one operation's classification, e.g. `summon[Matcher.planFor[SampleApi, "ping"].ArityInfo =:=
   * ArityTag.Fire]`, instead of comparing a runtime value with `assertEquals`. The returned value is a
   * `null` dummy: nothing here is ever meant to be called/dereferenced at runtime, only its STATIC
   * type — a plain type projection — is used.
   */
  transparent inline def planFor[T: {Done.Of as done, RpcNames as names}, L <: String]: OpPlan =
    ${ planForImpl[T, L]('done, 'names) }

  private def planForImpl[T: Type, L: Type](done: Expr[Done.Of[T]], names: Expr[RpcNames[T]])(using Quotes)
    : Expr[OpPlan] =
    import quotes.reflect.*
    val ops = done match
      case '{ type operations <: Tuple; $_ : { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]
    val resolvedNames = RpcNames.namesOf[T](names)
    val label = Type.valueOfConstant[L].getOrElse(report.errorAndAbort("L must be a literal string")).toString
    val idx = ops.indexWhere(op => OpReflect.labelOf(op) == label)
    if idx < 0 then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    planOne((ops(idx), resolvedNames(idx))) match
      case '[type p <: OpPlan; p] => '{ null.asInstanceOf[p] }
      case _ => report.errorAndAbort(s"could not build OpPlan for label '$label'")

  /**
   * Like [[planFor]], but returns EVERY operation sharing label `L` as a tuple (declaration order) —
   * needed when a label is shared by more than one op (overloads), to compare their [[OpPlan]]s
   * against each other at the type level (e.g. asserting their resolved `RpcName`s differ via
   * `scala.util.NotGiven`). `transparent inline`, so destructuring the tuple (`val (a, b) =
   * plansFor[...]`) gives each bound val its OWN precise `OpPlan` type, same as [[planFor]] does for a
   * single operation.
   */
  transparent inline def plansFor[T: {Done.Of as done, RpcNames as names}, L <: String]: Tuple =
    ${ plansForImpl[T, L]('done, 'names) }

  private def plansForImpl[T: Type, L: Type](done: Expr[Done.Of[T]], names: Expr[RpcNames[T]])(using Quotes)
    : Expr[Tuple] =
    import quotes.reflect.*
    val ops = done match
      case '{ type operations <: Tuple; $_ : { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]
    val resolvedNames = RpcNames.namesOf[T](names)
    val label = Type.valueOfConstant[L].getOrElse(report.errorAndAbort("L must be a literal string")).toString
    val matches = ops.zip(resolvedNames).filter((op, _) => OpReflect.labelOf(op) == label)
    if matches.isEmpty then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    val nulls: List[Expr[Any]] = matches.map(planOne).map { case '[type p <: OpPlan; p] =>
      '{ null.asInstanceOf[p] }
    }
    Expr.ofRefinedTuple(nulls)

  /** Classifies a single operation type into a refined [[OpPlan]] type using its resolved rpcName. */
  private def planOne(opAndName: (Type[?], Type[? <: String]))(using Quotes): Type[?] =
    import quotes.reflect.*
    val (opType, rpcName) = opAndName
    val arityType = opType match
      case '[{ type OutputType = Unit }] => Type.of[ArityTag.Fire]
      case '[{ type OutputType = Future[x] }] => Type.of[ArityTag.CallOf[x]]
      case '[{ type OutputType = other }] => Type.of[ArityTag.GetOf[other]]
    val paramsType: Type[? <: Tuple] = TupleTraverse.foldTuple(opType match
      case '[type elems <: Tuple; DoneOperation { type InputElems = elems }] =>
        TupleTraverse.traverseTuple[elems, InputElem].map(paramOf).map(planParam))

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
   * Per-param encode-vs-verbatim plan. Documented mrpc divergence from commons: commons makes
   * `@single`/`@optional` params verbatim by default because its raw type is concrete and often
   * equals the param type. mrpc keeps `Raw` abstract, so the only way a value reaches `Raw` is the
   * leaf codec bridge — every value param is `Encoded` by default. `@verbatim` yields `Verbatim` ONLY
   * when the param's declared type already IS the abstract `Raw` (rare; e.g. a pass-through transport).
   * Since `Raw` is not in scope as a concrete type in this standalone matcher, the verbatim branch is
   * gated on `@verbatim` being present AND the param type being an abstract/opaque carrier; in the
   * fixed-String fixtures no param is `Raw`, so this resolves to `Encoded`, matching the divergence.
   */
  private def planParam(param: Type[?])(using Quotes): Type[?] = param match
    case '[Param { type ParamType = t }] =>
      val encodingType: Type[? <: EncodingTag] =
        if OpReflect.paramHasVerbatim(param) && OpReflect.isRawCarrier[t] then Type.of[EncodingTag.Verbatim]
        else Type.of[EncodingTag.Encoded]
      (param, encodingType) match
        case ('[type l <: String; { type Label = l }], '[type e <: EncodingTag; e]) =>
          Type.of[ParamPlan { type Label = l; type ParamType = t; type Encoding = e }]
        case _ => quotes.reflect.report.errorAndAbort(s"could not build ParamPlan for param")
