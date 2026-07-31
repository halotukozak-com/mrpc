package mrpc.derive

import made.{Done, DoneOperation, InputElem, Meta}
import mrpc.annotation.verbatim

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

  /** The op's (or param's) `Label` singleton-string member as a plain `String`. */
  private def labelOf(opType: Type[?])(using Quotes): String =
    opType.runtimeChecked match
      case '[type l <: String; { type Label = l }] =>
        Type.valueOfConstant[l].getOrElse(quotes.reflect.report.errorAndAbort("Label is not a string literal")).toString

  /**
   * Classifies every operation of `T` into an [[OpPlan]] — one per `Done.Operations` entry, in the
   * same order — the consumer-facing list the server adapter and client proxy macros iterate over,
   * each queried on demand via local quote-pattern reads on the plan's `Type[?]`. rpcName resolution
   * (incl. overload disambiguation + duplicate detection) is delegated to [[RpcNames]]. The `Done`
   * mirror is passed in (summoned once at the call site via the `Done.Of as done` context bound), not
   * re-summoned here.
   */
  def plans[T: Type](done: Expr[Done.Of[T]], names: Expr[RpcNames[T]])(using Quotes): List[Type[? <: OpPlan]] =
    val ops = done match
      case '{ type operations <: Tuple; $_ : { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]
    // Names come from the single authority `RpcNames[T]` (type-level `Names`, read back by `namesOf`);
    // `RpcNames.derived`'s own `errorAndAbort` on a duplicate name surfaces at its summon site.
    val resolvedNames = RpcNames.namesOf[T](names)
    ops.zip(resolvedNames).map(planOne)

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
    val idx = ops.indexWhere(op => labelOf(op) == label)
    if idx < 0 then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    planOne(ops(idx), resolvedNames(idx)) match
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
    val matches = ops.zip(resolvedNames).filter((op, _) => labelOf(op) == label)
    if matches.isEmpty then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    val nulls: List[Expr[Any]] = matches.map(planOne).map { case '[type p <: OpPlan; p] =>
      '{ null.asInstanceOf[p] }
    }
    Expr.ofRefinedTuple(nulls)

  /** Classifies a single operation type into a refined [[OpPlan]] type using its resolved rpcName. */
  private def planOne(opType: Type[?], rpcName: Type[? <: String])(using Quotes): Type[? <: OpPlan] =
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
              case AnnotatedType(_, annot) => annot.tpe <:< TypeRepr.of[verbatim]
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
