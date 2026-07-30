package mrpc.derive

import scala.concurrent.Future
import scala.quoted.*

import made.Done

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
 * (matching/dispatch) and metadata materialization — one Done-walk path, no fork. The cross-package
 * metadata suite also asserts the metadata rpcNames EQUAL [[describe]]'s.
 */
private[mrpc] object Matcher:

  /**
   * Test-facing entry point: classifies every operation of `T` and yields the flattened, runtime
   * [[OpDescriptor]] list the suites assert against. The full `OpPlan` (with its `Type[?]` payloads)
   * stays inside the macro; only the comparable projection escapes.
   */
  inline def describe[T]: List[OpDescriptor] = ${ describeImpl[T] }

  private def describeImpl[T: Type](using Quotes): Expr[List[OpDescriptor]] =
    val descriptors = plans[T](summonDone[T]).map(descriptorExpr)
    Expr.ofList(descriptors)

  /** Renders one [[OpPlan]] type into the runtime [[OpDescriptor]] the tests compare against. */
  private def descriptorExpr(plan: Type[?])(using Quotes): Expr[OpDescriptor] =
    val (arityTag, carried) = PlanReflect.arityOf(plan) match
      case '[ArityTag.Fire] => ("fire", "")
      case '[ArityTag.CallOf[r]] => ("call", typeShow(Type.of[r]))
      case '[ArityTag.GetOf[s]] => ("get", typeShow(Type.of[s]))
      case _ => quotes.reflect.report.errorAndAbort("unrecognized ArityTag")
    val paramEncodings = PlanReflect.paramsOf(plan).map(p => encodingTag(p.encoding))
    '{
      OpDescriptor(
        label = ${ Expr(PlanReflect.labelOf(plan)) },
        rpcName = ${ Expr(PlanReflect.rpcNameOf(plan)) },
        arity = ${ Expr(arityTag) },
        carriedType = ${ Expr(carried) },
        paramEncodings = ${ Expr(paramEncodings) },
        resultEncoding = ${ Expr(encodingTag(PlanReflect.resultEncodingOf(plan))) },
      )
    }

  private def encodingTag(tag: Type[?])(using Quotes): String = tag match
    case '[EncodingTag.Encoded] => "encoded"
    case '[EncodingTag.Verbatim] => "verbatim"
    case _ => quotes.reflect.report.errorAndAbort("unrecognized EncodingTag")

  private def typeShow(t: Type[?])(using Quotes): String =
    import quotes.reflect.*
    TypeRepr.of(using t).show

  /**
   * Summons the `Done.Of[T]` mirror ONCE. Callers thread the result into [[planAll]] /
   * [[operationTypes]] instead of re-summoning — the server path in particular needs the same mirror
   * both for classification and for `Done.invoke`, so a single summon avoids deriving it twice.
   */
  def summonDone[T: Type](using Quotes): Expr[Done.Of[T]] =
    import quotes.reflect.*
    Expr.summon[Done.Of[T]].getOrElse {
      report.errorAndAbort(s"could not summon a Done mirror for ${TypeRepr.of[T].show}")
    }

  /**
   * Classifies every operation of `T` into an [[OpPlan]] and folds the results into a `Plans <:
   * Tuple` type — one [[OpPlan]] per `Done.Operations` entry, in the same order — mirroring how
   * `Done` itself models `T` as a `Tuple` of `DoneOperation`s. rpcName resolution (incl. overload
   * disambiguation + duplicate detection) is delegated to [[RpcName.computeAll]]. The `Done` mirror
   * is passed in (summoned once at the entry point via [[summonDone]]), not re-summoned here.
   */
  def planAll[T: Type](done: Expr[Done.Of[T]])(using Quotes): Type[? <: Tuple] =
    val ops = operationTypes(done)
    // Names come from the single authority `RpcNames[T]` (type-level `Names`, read back by `namesOf`).
    // `namesOf` falls back to a direct `computeAll` when the mirror can't be summoned, so the
    // duplicate-name abort still surfaces verbatim (see CompileErrorSuite).
    val resolvedNames = RpcNames.namesOf[T](RpcNames.summonNames[T])
    val planTypes: List[Type[?]] = ops.zip(resolvedNames).map(planOne)
    TupleTraverse.foldTuple(planTypes)

  /**
   * The consumer-facing entry point: [[planAll]]'s `Plans` tuple, traversed back into the
   * `List[Type[?]]` of individual [[OpPlan]]s the server adapter and client proxy macros actually
   * iterate over — each queried on demand via [[PlanReflect]], same as [[operationTypes]]'s list of
   * `DoneOperation` types is queried via [[OpReflect]].
   */
  def plans[T: Type](done: Expr[Done.Of[T]])(using Quotes): List[Type[?]] =
    planAll[T](done) match
      case '[type ps <: Tuple; ps] => TupleTraverse.traverseTuple(Type.of[ps])

  /**
   * Exposes a SINGLE operation's [[OpPlan]] type directly to the CALLER's type checker —
   * `transparent inline`, the same technique `made.Done.derived` uses to make its refined type
   * visible outside the macro. Lets ordinary, non-macro code (tests) assert compile-time facts about
   * one operation's classification, e.g. `summon[Matcher.planFor[SampleApi, "ping"].ArityInfo =:=
   * ArityTag.Fire]`, instead of comparing a runtime [[OpDescriptor]] with `assertEquals`. The returned
   * value is a `null` dummy: nothing here is ever meant to be called/dereferenced at runtime, only its
   * STATIC type — a plain type projection — is used.
   */
  transparent inline def planFor[T, L <: String]: OpPlan = ${ planForImpl[T, L] }

  private def planForImpl[T: Type, L: Type](using Quotes): Expr[OpPlan] =
    import quotes.reflect.*
    val done = summonDone[T]
    val ops = operationTypes(done)
    val resolvedNames = RpcNames.namesOf[T](RpcNames.summonNames[T])
    val label = Type.valueOfConstant[L].getOrElse(report.errorAndAbort("L must be a literal string")).toString
    val idx = ops.indexWhere(op => OpReflect.labelOf(op) == label)
    if idx < 0 then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    planOne((ops(idx), resolvedNames(idx))) match
      case '[type p <: OpPlan; p] => '{ null.asInstanceOf[p] }
      case _ => report.errorAndAbort(s"could not build OpPlan for label '$label'")

  /** Extracts the refined `DoneOperation` element types from the (passed-in) mirror's `Operations`. */
  private[mrpc] def operationTypes[T: Type](doneExpr: Expr[Done.Of[T]])(using Quotes): List[Type[?]] =
    import quotes.reflect.*
    val doneTpe = doneExpr.asTerm.tpe.widen
    val operationsTpe = doneTpe.select(doneTpe.typeSymbol.typeMember("Operations")).dealias
    operationsTpe.asType match
      case '[type ops <: Tuple; ops] => TupleTraverse.traverseTuple(Type.of[ops])
      case _ => report.errorAndAbort("Done.Operations is not a tuple")

  /** Classifies a single operation type into a refined [[OpPlan]] type using its resolved rpcName. */
  private def planOne(opAndName: (Type[?], String))(using Quotes): Type[?] =
    import quotes.reflect.*
    val (opType, rpcName) = opAndName
    val arityType: Type[? <: ArityTag] = arityOf(OpReflect.outputType(opType))
    val paramTypes: List[Type[?]] = OpReflect.inputElems(opType).map(planParam)
    val paramsType: Type[? <: Tuple] = TupleTraverse.foldTuple(paramTypes)

    // Under the fixed-RawRpc model the result is always encoded via the summoned leaf bridge unless
    // it is itself Raw; @verbatim on a non-Raw result has no identity to land on. Result-verbatim is
    // therefore only reachable through the same Raw-type check params use; v1 fixtures encode results.
    // `label` relays the op's EXISTING `Label` type directly (`labelTypeOf`); `rpcName` has no
    // pre-existing type to relay — it's a value computed by `RpcName.computeAll` — so it genuinely
    // needs lifting via `ConstantType(StringConstant(...))`.
    (OpReflect.labelTypeOf(opType), ConstantType(StringConstant(rpcName)).asType, arityType, paramsType, opType) match
      case (
            '[type l <: String; l],
            '[type n <: String; n],
            '[type a <: ArityTag; a],
            '[type ps <: Tuple; ps],
            '[o],
          ) =>
        Type.of[OpPlan {
          type Label = l
          type RpcName = n
          type ArityInfo = a
          type Params = ps
          type ResultEncoding = EncodingTag.Encoded
          type OpType = o
        }]
      case _ => report.errorAndAbort(s"could not build OpPlan for operation ${TypeRepr.of(using opType).show}")

  /**
   * Arity from the output type. `Unit` -> fire; `Future[X]` -> call carrying `X`; anything else is
   * treated as a sub-RPC getter seam (recursion deferred). The unsupported-result-type compile error
   * fires at the materialize site where a sub-RPC conversion cannot be summoned (see CompileErrorSuite).
   */
  private def arityOf(output: Type[?])(using Quotes): Type[? <: ArityTag] =
    output match
      case '[Unit] => Type.of[ArityTag.Fire]
      case '[Future[x]] => Type.of[ArityTag.CallOf[x]]
      case '[other] => Type.of[ArityTag.GetOf[other]]

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
  private def planParam(param: Type[?])(using Quotes): Type[?] =
    val paramType = OpReflect.paramTypeOf(param)
    val encodingType: Type[? <: EncodingTag] =
      if OpReflect.paramHasVerbatim(param) && OpReflect.isRawCarrier(paramType) then Type.of[EncodingTag.Verbatim]
      else Type.of[EncodingTag.Encoded]
    (OpReflect.labelTypeOf(param), paramType, encodingType) match
      case ('[type l <: String; l], '[t], '[type e <: EncodingTag; e]) =>
        Type.of[ParamPlan { type Label = l; type ParamType = t; type Encoding = e }]
      case _ => quotes.reflect.report.errorAndAbort(s"could not build ParamPlan for param")
