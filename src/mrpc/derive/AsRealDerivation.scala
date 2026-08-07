package mrpc
package derive

import made.*
import mrpc.conv.AsReal
import mrpc.raw.RawRpc

import scala.concurrent.ExecutionContext

/**
 * Client-proxy derivation: builds an `AsReal[RawRpc[Raw], Real]` that turns a transport-facing
 * [[RawRpc]] into a concrete implementation of the real trait.
 *
 * The trait synthesis is delegated to made's `Done.materialize` (the tuple-of-handlers `.to[Real]`):
 * one [[Handler]] per operation — each an `op.Args => op.OutputType` function that packages a
 * `RawInvocation` and forwards to the underlying `RawRpc[Raw]`'s `fire`/`call`/`get` by arity,
 * decoding the result back via a summoned `AsReal`. made wires method i to handler i (declaration
 * order = `Done.Operations` order = `Plans[Real].Underlying` order), so mrpc carries no proxy macro
 * of its own.
 *
 * Arity routing and per-handler body live in [[Handler]]; this object's only job is to read every
 * classified [[OpPlan]] off [[Plans]] and assemble the handler tuple `made.Done.materialize` expects.
 */
object AsRealDerivation:

  inline def impl[Raw, Real: {Done.Of as done}](plans: Tuple)(using ExecutionContext): AsReal[RawRpc[Raw], Real] =
    raw => buildAllHandlers[Raw, plans.type](using raw, containsOnly.refl).to[Real](using done)(using ValidHandlers.refl)

  transparent inline private def buildAllHandlers[Raw: RawRpc, Plans <: Tuple](
    using Plans containsOnly OpPlan,
    ExecutionContext,
  ): Tuple = inline compiletime.erasedValue[Plans] match
    case _: EmptyTuple => EmptyTuple
    case _: (head *: tail) =>
      realCons(Handler.materialize[Raw, head & OpPlan], buildAllHandlers[Raw, tail & Tuple.Tail[Plans]])
