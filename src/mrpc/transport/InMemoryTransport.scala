package mrpc.transport

import scala.concurrent.Future

import mrpc.raw.{RawInvocation, RawRpc}

/**
 * The one in-memory `RawRpc[String]` instantiation: a thin wrapper over a server-side
 * `RawRpc[String]` (the materialized server adapter) that routes every `fire`/`call`/`get` across an
 * explicit transport boundary. Because `Raw = String` (JSON) the invocation is already fully
 * serializable, so the "transport" is in-process — a function call across the [[hop]] step rather than
 * an HTTP/WebSocket stack. No new `Raw` type and no codec are introduced; the leaf encoding stays in
 * `mrpc.codec.JsonRawValue`.
 *
 * This adds the explicit raw->transport->raw hop the bare loopback lacks, completing a full
 * real->raw->transport->raw->real round-trip when sandwiched between `materializeAsRaw.asRaw` and
 * `materializeAsReal.asReal`.
 *
 * Crucially, [[get]] re-wraps the sub-RPC in a fresh `InMemoryTransport`, so sub-RPC recursion
 * re-crosses the boundary at every nesting level rather than only at the top call.
 */
final class InMemoryTransport(server: RawRpc[String]) extends RawRpc[String]:

  /**
   * Carries the invocation across the (in-process) transport boundary. Since `Raw = String` is already
   * serializable, the boundary is made observable by reconstructing the value from its transported
   * fields — an explicit hand-off rather than passing the same reference straight through.
   */
  private def hop(invocation: RawInvocation[String]): RawInvocation[String] =
    RawInvocation(invocation.rpcName, invocation.args, invocation.metadata)

  def fire(invocation: RawInvocation[String]): Unit = server.fire(hop(invocation))

  def call(invocation: RawInvocation[String]): Future[String] = server.call(hop(invocation))

  // Re-wrap the sub-RPC so each nesting level re-crosses the transport boundary (recursion-aware).
  def get(invocation: RawInvocation[String]): RawRpc[String] =
    new InMemoryTransport(server.get(hop(invocation)))

object InMemoryTransport:
  /** Ergonomic construction mirroring the `new InMemoryTransport(server)` form. */
  def apply(server: RawRpc[String]): InMemoryTransport = new InMemoryTransport(server)
