package halotukozak.mrpc
package derive

import commons.{containsOnly, realCons, Of}
import made.{Done, DoneOperation}
import halotukozak.mrpc.conv.{AsRaw, AsReal}
import halotukozak.mrpc.raw.{RawInvocation, RawRpc}

import scala.concurrent.{ExecutionContext, Future}

/**
 * Server-adapter derivation: builds an `AsRaw[RawRpc[Raw], Real]` that turns a real trait instance
 * into a transport-facing [[RawRpc]]. The generated `RawRpc[Raw]` dispatches each incoming
 * [[RawInvocation]] by `rpcName`, decodes every argument to its exact declared parameter type via a
 * summoned `AsReal[Raw, paramType]`, calls `made.Done.invoke`, and encodes the result back to `Raw`.
 *
 * The `fire`/`call`/`get` match arms are partitioned by arity — only `fire`-arity ops appear in
 * `fire`, and so on. An `rpcName` outside an arity's known set is rejected explicitly (no silent
 * no-op), mirroring commons' unknown-method handling.
 */
object AsRawDerivation:

  inline def impl[Raw, Real: Done.Of](plans: Tuple)(using ExecutionContext): AsRaw[RawRpc[Raw], Real] =
    (api: Real) =>
      given plans.type containsOnly OpPlan = containsOnly.refl
      buildRawRpc[Raw, Real, plans.type](api)

  /** Assembles the dispatching `RawRpc[Raw]`; `Done.Of[Real]` is summoned once and shared across `fire`/`call`/`get`. */
  inline def buildRawRpc[Raw, Real: {Done.Of as done}, Plans <: Tuple: Of[OpPlan]](api: Real)(using ExecutionContext)
    : RawRpc[Raw] =
    type Names = Tuple.Map[
      Plans,
      [op] =>> op match
        case ([n] =>> OpPlan { type RpcName = n })[name] => name,
    ]
    mkRawRpc[Raw](
      fireBody[Raw, Real, Plans, Names](api),
      callBody[Raw, Real, Plans, Names](api),
      getBody[Raw, Real, Plans, Names](api),
    )

  /** Not `inline` — an anonymous class defined directly inside an inline body would recompile at every call site. */
  private def mkRawRpc[Raw](
    fireFn: RawInvocation[Raw] => Unit,
    callFn: RawInvocation[Raw] => Future[Raw],
    getFn: RawInvocation[Raw] => RawRpc[Raw],
  ): RawRpc[Raw] = new:
    def fire(invocation: RawInvocation[Raw]): Unit = fireFn(invocation)
    def call(invocation: RawInvocation[Raw]): Future[Raw] = callFn(invocation)
    def get(invocation: RawInvocation[Raw]): RawRpc[Raw] = getFn(invocation)

  // --- arity-partitioned dispatch bodies ---

  /**
   * `fireArms`/`callArms`/`bodyArms` each walk `Acc` (= `Plans`) directly, matching each op's own
   * `ArityInfo` inline. This can NOT go through a shared helper taking a `[A <: ArityTag, ...] => ...`
   * callback: passing the concrete arity type through a polymorphic function value's own type
   * parameter and re-matching it inside that value's body does not preserve its concreteness — the
   * inner match always falls to `case _ =>`, for every op, regardless of its real arity.
   *
   * Every value built here spans EVERY entry of `Acc`, not just this body's own arity, positionally
   * aligned with the shared `Names` (computed once in `buildRawRpc`): an op outside this body's own
   * arity gets a `reject` thunk in its slot instead of real dispatch logic, so `Names` and the values
   * tuple can never drift apart via separate filtering.
   *
   * Each value is a THUNK (`RawInvocation[Raw] => Result`), not the invocation itself: `matchFrom`
   * builds `case name => args(index)` over the whole values tuple, so if the values were eager,
   * constructing that tuple would decode and invoke every op on every dispatch, not just the one
   * whose name matched. The thunk defers that to the final `(inv)` application below.
   */
  transparent inline private def fireArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  )(
    index: Int,
  ): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        val arm = inline compiletime.erasedValue[head & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case ArityTag.Fire =>
                (inv: RawInvocation[Raw]) => invoke[Raw, Real, Any, op.type, op.Args](api, inv, index): Unit
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        realCons(arm, fireArms[Raw, Real, tail](api)(index + 1))

  inline private def fireBody[Raw, Real, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  ): Unit = matchFrom(NamedTuple.build[Names]()(fireArms[Raw, Real, Plans](api)(0)))[RawInvocation[Raw] => Unit](
    inv.rpcName,
    reject,
  )(inv)

  transparent inline private def callArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  )(
    index: Int,
  )(using ec: ExecutionContext,
  ): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        val arm = inline compiletime.erasedValue[head & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case _: ArityTag.Call[r] =>
                (inv: RawInvocation[Raw]) =>
                  val futureEncoder = AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], ec)
                  futureEncoder.asRaw(invoke[Raw, Real, Future[r], op.type, op.Args](api, inv, index))
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        realCons(arm, callArms[Raw, Real, tail](api)(index + 1))

  inline private def callBody[Raw, Real, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  )(using ExecutionContext,
  ): Future[Raw] = matchFrom(
    NamedTuple.build[Names]()(callArms[Raw, Real, Plans](api)(0)),
  )[RawInvocation[Raw] => Future[Raw]](inv.rpcName, reject)(inv)

  transparent inline private def bodyArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  )(
    index: Int,
  ): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        val arm = inline compiletime.erasedValue[head & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case _: ArityTag.Get[sub] =>
                (inv: RawInvocation[Raw]) =>
                  AsRaw
                    .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
                    .asRaw(invoke[Raw, Real, sub, op.type, op.Args](api, inv, index))
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        realCons(arm, bodyArms[Raw, Real, tail](api)(index + 1))

  inline private def getBody[Raw, Real, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  ): RawRpc[Raw] =
    matchFrom(
      NamedTuple.build[Names]()(bodyArms[Raw, Real, Plans](api)(0)),
    )[RawInvocation[Raw] => RawRpc[Raw]](inv.rpcName, reject)(inv)

  // --- shared helpers ---

  inline private def invoke[Raw, Real: Done.Of as done, R, Plan <: OpPlan, Args <: Tuple](
    api: Real,
    inv: RawInvocation[Raw],
    index: Int,
  ): R =
    val operation = done.operations(index).asInstanceOf[DoneOperation { type OuterType = Real }]
    val argsTuple = decodedArgs[Raw, Args](inv.args.flatten)(0)
    done.invoke(operation, api, argsTuple.asInstanceOf[operation.Args]).asInstanceOf[R]

  transparent inline private def decodedArgs[Raw, Acc <: Tuple](flatArgs: Seq[Raw])(i: Int): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (head *: tail) =>
        realCons(
          scala.compiletime.summonInline[AsReal[Raw, head]].asReal(flatArgs(i)),
          decodedArgs[Raw, tail](flatArgs)(i + 1),
        )

  private def reject(inv: RawInvocation[?]): Nothing =
    throw new IllegalArgumentException("unknown rpc name: " + inv.rpcName)
