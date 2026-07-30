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
    val plans = planAll[T](summonDone[T])
    val descriptors = plans.map(descriptorExpr)
    Expr.ofList(descriptors)

  /** Renders one [[OpPlan]] into the runtime [[OpDescriptor]] the tests compare against. */
  private def descriptorExpr(plan: OpPlan)(using Quotes): Expr[OpDescriptor] =
    val arityTag = plan.arity match
      case Arity.Fire => "fire"
      case Arity.Call(_) => "call"
      case Arity.Get(_) => "get"
    val carried = plan.arity match
      case Arity.Fire => ""
      case Arity.Call(t) => typeShow(t)
      case Arity.Get(t) => typeShow(t)
    val paramEncodings = plan.params.map(p => encodingTag(p.encoding))
    '{
      OpDescriptor(
        label = ${ Expr(plan.label) },
        rpcName = ${ Expr(plan.rpcName) },
        arity = ${ Expr(arityTag) },
        carriedType = ${ Expr(carried) },
        paramEncodings = ${ Expr(paramEncodings) },
        resultEncoding = ${ Expr(encodingTag(plan.resultEncoding)) },
      )
    }

  private def encodingTag(e: Encoding): String = e match
    case Encoding.Encoded => "encoded"
    case Encoding.Verbatim => "verbatim"

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
   * Classifies every operation of `T` into an [[OpPlan]], delegating rpcName resolution (incl.
   * overload disambiguation + duplicate detection) to [[RpcName.computeAll]]. The `Done` mirror is
   * passed in (summoned once at the entry point via [[summonDone]]), not re-summoned here.
   */
  def planAll[T: Type](done: Expr[Done.Of[T]])(using Quotes): List[OpPlan] =
    val ops = operationTypes(done)
    // Names come from the single authority `RpcNames[T]` (type-level `Names`, read back by `namesOf`).
    // `namesOf` falls back to a direct `computeAll` when the mirror can't be summoned, so the
    // duplicate-name abort still surfaces verbatim (see CompileErrorSuite).
    val resolvedNames = RpcNames.namesOf[T](RpcNames.summonNames[T])
    ops.zip(resolvedNames).zipWithIndex.map { case ((op, name), index) =>
      planOne(op, name, index)
    }

  /** Extracts the refined `DoneOperation` element types from the (passed-in) mirror's `Operations`. */
  private[mrpc] def operationTypes[T: Type](doneExpr: Expr[Done.Of[T]])(using Quotes): List[Type[?]] =
    import quotes.reflect.*
    val doneTpe = doneExpr.asTerm.tpe.widen
    val operationsTpe = doneTpe.select(doneTpe.typeSymbol.typeMember("Operations")).dealias
    operationsTpe.asType match
      case '[type ops <: Tuple; ops] => TupleTraverse.traverseTuple(Type.of[ops])
      case _ => report.errorAndAbort("Done.Operations is not a tuple")

  /** Classifies a single operation type into an [[OpPlan]] using its already-resolved rpcName. */
  private def planOne(opType: Type[?], rpcName: String, index: Int)(using Quotes): OpPlan =
    val label = OpReflect.labelOf(opType)
    val arity = arityOf(OpReflect.outputType(opType))
    val params = OpReflect.inputElems(opType).map(planParam)
    // Under the fixed-RawRpc model the result is always encoded via the summoned leaf bridge unless
    // it is itself Raw; @verbatim on a non-Raw result has no identity to land on. Result-verbatim is
    // therefore only reachable through the same Raw-type check params use; v1 fixtures encode results.
    val resultEncoding = Encoding.Encoded
    OpPlan(label, rpcName, arity, params, resultEncoding, opType, index)

  /**
   * Arity from the output type. `Unit` -> fire; `Future[X]` -> call carrying `X`; anything else is
   * treated as a sub-RPC getter seam (recursion deferred). The unsupported-result-type compile error
   * fires at the materialize site where a sub-RPC conversion cannot be summoned (see CompileErrorSuite).
   */
  private def arityOf(output: Type[?])(using Quotes): Arity =
    output match
      case '[Unit] => Arity.Fire
      case '[Future[x]] => Arity.Call(Type.of[x])
      case '[other] => Arity.Get(Type.of[other])

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
  private def planParam(param: OpReflect.Param)(using Quotes): ParamPlan =
    val encoding =
      if param.hasVerbatim && OpReflect.isRawCarrier(param.tpe) then Encoding.Verbatim
      else Encoding.Encoded
    ParamPlan(param.label, param.tpe, encoding)
