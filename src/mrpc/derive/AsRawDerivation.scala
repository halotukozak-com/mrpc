package mrpc
package derive

import made.{containsOnly, Done, DoneOperation}
import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}
import mrpc.realCons

import scala.concurrent.{ExecutionContext, Future}

/**
 * Server-adapter derivation: builds an `AsRaw[RawRpc[Raw], Real]` that turns a real trait instance
 * into a transport-facing [[RawRpc]]. The generated `RawRpc[Raw]` dispatches each incoming
 * [[RawInvocation]] by `rpcName`, decodes every argument to its EXACT declared parameter type via a
 * summoned `AsReal[Raw, paramType]`, calls `made.Done.invoke`, and encodes the result back to `Raw`.
 *
 * Decoding to the exact `InputElem.Type` before tupling is what makes made's internal unboxing cast
 * (in `DoneOperation.apply`) a provable no-op: the value handed to the tuple is already statically
 * the declared type, so no boxing/unboxing mismatch can crash even on primitives or wrapper types.
 *
 * The `match` arms are PARTITIONED BY ARITY — only `fire`-arity ops appear in `fire`, only
 * `call`-arity in `call`, only `get`-arity in `get`. An `rpcName` outside an arity's known set falls
 * through to an explicit rejection (no silent no-op), mirroring commons' unknown-method handling.
 */
object AsRawDerivation:

  /**
   * The server-adapter conversion as a plain value: `asRaw` wraps a `Real` in the `RawRpc[Raw]` built
   * by [[buildRawRpc]]. The `AsRaw` wrapper is ordinary Scala (a SAM lambda); only the `RawRpc` itself
   * — its arity-partitioned `fire`/`call`/`get` dispatch — is generated.
   */
  inline def impl[Raw, Real: Done.Of](plans: Plans[Real])(using ExecutionContext): AsRaw[RawRpc[Raw], Real] =
    (api: Real) => buildRawRpc[Raw, Real, plans.Underlying](api)

  /**
   * Macro entry: assembles the dispatching `RawRpc[Raw]` for a `Real` instance. `Done.Of[Real]` and
   * `Plans[Real]` are summoned ONCE here (via their own context bounds) and shared across the
   * `fire`/`call`/`get` bodies below — unlike three independent macro entries, which would each
   * re-derive them.
   */
  inline private def buildRawRpc[Raw, Real: {Done.Of as done}, Plans <: Tuple](
    api: Real,
  )(using Plans containsOnly OpPlan,
  )(using ec: ExecutionContext,
  ): RawRpc[Raw] =
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

  /**
   * `mkRawRpc` is an ordinary, non-`inline` method — an anonymous class defined directly inside an
   * `inline`/macro body is otherwise recompiled at every inline site.
   */
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
   * Each of `fireArms`/`callArms`/`bodyArms` walks `Acc` (= `Plans`) directly, matching each op's OWN
   * `ArityInfo` inline — NOT through a shared generic `arms` helper taking a `[A <: ArityTag, ...] =>
   * ...` callback: that indirection lost `A`'s concreteness. `op.ArityInfo` reads as the real, concrete
   * arity tag right here (proven directly), but the SAME type, passed through a polymorphic function
   * VALUE's own type parameter and re-matched INSIDE that value's body, no longer resolved to a
   * concrete case there — the inner match always fell to its `case _ =>` filler branch, for every op,
   * regardless of its real arity. Duplicating the ~10-line recursion three times avoids that layer.
   *
   * Every value built here is over EVERY entry of `Acc` (not just this body's own arity), positionally
   * aligned with the shared `Names` (computed once in `buildRawRpc`) by construction — an op outside
   * this body's own arity gets a `reject` thunk in its slot instead of real dispatch logic. This is
   * what keeps `Names`/`values` from silently drifting apart: there is no separate filter step to fall
   * out of sync with the other's filter.
   *
   * Every value is a THUNK (`RawInvocation[Raw] => Result`), not the invocation itself: `matchFrom`
   * builds `case name => args(index)` over the WHOLE `values` tuple, and constructing that tuple at all
   * would otherwise force every op's `invoke` — decoding `inv`'s args against every op's own, generally
   * mismatched, parameter types, and running every op's real implementation — on every single dispatch,
   * not just the one op whose name matched. Thunking defers all of that to the final `(inv)` application
   * below, which only ever runs the ONE arm `matchFrom` actually selected.
   */
  transparent inline private def fireArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  )(
    index: Int,
  ): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val arm = inline compiletime.erasedValue[h & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case _: ArityTag.Fire =>
                (inv: RawInvocation[Raw]) => invoke[Raw, Real, Any, op.type, op.Args](api, inv, index): Unit
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        fireArms[Raw, Real, t](api)(index + 1).realCons(arm)

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
      case _: (h *: t) =>
        val arm = inline compiletime.erasedValue[h & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case _: ArityTag.Call[r] =>
                (inv: RawInvocation[Raw]) =>
                  val futureEncoder = AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], ec)
                  futureEncoder.asRaw(invoke[Raw, Real, Future[r], op.type, op.Args](api, inv, index))
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        callArms[Raw, Real, t](api)(index + 1).realCons(arm)

  inline private def callBody[Raw, Real: Done.Of, Plans <: Tuple, Names <: Tuple](
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
      case _: (h *: t) =>
        val arm = inline compiletime.erasedValue[h & OpPlan] match
          case op =>
            inline compiletime.erasedValue[op.ArityInfo] match
              case _: ArityTag.Get[sub] =>
                (inv: RawInvocation[Raw]) =>
                  AsRaw
                    .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
                    .asRaw(invoke[Raw, Real, sub, op.type, op.Args](api, inv, index))
              case _ => (inv: RawInvocation[Raw]) => reject(inv)
        bodyArms[Raw, Real, t](api)(index + 1).realCons(arm)

  inline private def getBody[Raw, Real: Done.Of, Plans <: Tuple, Names <: Tuple](
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

  private transparent inline def decodedArgs[Raw, Acc <: Tuple](flatArgs: List[Raw])(i: Int): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: tail) =>
        scala.compiletime.summonInline[AsReal[Raw, h]].asReal(flatArgs(i)) *: decodedArgs[Raw, tail](flatArgs)(i + 1)

  private def reject(inv: RawInvocation[?]): Nothing =
    throw new IllegalArgumentException("unknown rpc name: " + inv.rpcName)
