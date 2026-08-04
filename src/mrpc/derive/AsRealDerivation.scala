package mrpc.derive

import made.*
import mrpc.conv.AsReal
import mrpc.raw.RawRpc

import scala.annotation.unused
import scala.concurrent.ExecutionContext

/**
 * Client-proxy derivation: builds an `AsReal[RawRpc[Raw], Real]` that turns a transport-facing
 * [[RawRpc]] into a concrete implementation of the real trait.
 *
 * The trait synthesis is delegated to made's `Done.materialize` (the tuple-of-handlers `.to[Real]`):
 * one [[Handler]] per operation — each an `op.Args => op.OutputType` function that packages a
 * `RawInvocation` (the resolved `rpcName` + per-param-list encoded arguments) and forwards to the
 * underlying `RawRpc[Raw]`'s `fire`/`call`/`get` by arity, decoding the result back to the method's
 * exact declared type via a summoned `AsReal`. made wires method i to handler i (declaration order =
 * `Done.Operations` order = `Plans[Real].All` order), so mrpc no longer carries its own
 * `Symbol.newClass` proxy macro.
 *
 * Arity routing and per-handler body live in [[Handler]]; this object's only job is to read every
 * classified [[OpPlan]] off [[Plans]] and assemble the handler tuple `made.Done.materialize` expects.
 */
object AsRealDerivation:

  /**
   * The client-proxy conversion as a plain value: `asReal` turns a `RawRpc[Raw]` into a `Real` proxy
   * via the [[materializeProxy]] macro. The `AsReal` wrapper itself is ordinary Scala; only the
   * per-`raw` proxy body is generated. `ExecutionContext` is in scope so the `call` arity can compose
   * `AsReal[Future[Raw], Future[r]]` via `forFuture`.
   */
  inline def impl[Raw, Real: Done.Of](using ExecutionContext): AsReal[RawRpc[Raw], Real] =
    raw =>
      given RawRpc[Raw] = raw
      materializeProxy[Raw, Real]

  /**
   * Builds the `Real` client proxy for a single `RawRpc[Raw]`. Deliberately NOT itself a macro (no
   * top-level `${...}` here): `.to[Real]` is made's OWN macro (`transparent inline def to[Target:
   * Done.Of as done](using ValidHandlers[done.Operations, Handlers])`), and calling it from INSIDE
   * another macro's generated tree (rather than from plain inline-def source, as here) reintroduces
   * the well-known `done$proxyN` inliner-proxy mismatch — two different views of the same
   * context-bound `done` disagreeing on `Operations`'s precise type. Only the handler TUPLE itself
   * (whose per-op shape genuinely needs macro-level access to [[Plans]]) is built by [[buildHandlers]].
   */
  inline def materializeProxy[Raw: RawRpc, Real: {Done.Of as done, Plans as plans}](using ExecutionContext): Real =
    buildHandlers[Raw, Real](plans).to[Real](using done)(using ValidHandlers.refl)

  /**
   * Macro entry: reads every classified [[OpPlan]] off `Plans[Real].All` (via [[Plans.allOf]] — the
   * one macro-level access point, same as [[Matcher]] uses) and summons ONE `Handler[Raw, op]` per
   * position, assembled into the tuple made's `.to[Real]` expects.
   */
  inline private def buildHandlers[Raw: RawRpc as raw, Real](plans: Plans[Real])(using ec: ExecutionContext): Tuple =
    @unused given RawRpc[Raw] = raw
    @unused given ExecutionContext = ec
    compiletime.summonAll[Tuple.Map[plans.Underlying, [O] =>> Handler[Raw, O & OpPlan]]]
