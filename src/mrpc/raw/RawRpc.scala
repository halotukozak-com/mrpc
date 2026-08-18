package halotukozak.mrpc.raw

import scala.concurrent.Future

/**
 * Transport-agnostic raw RPC: arity-based dispatch selected (later) from operation OutputType.
 * fire = procedures (Unit), call = functions (Future[Raw]), get = sub-RPC getters.
 */
trait RawRpc[Raw]:
  def fire(invocation: RawInvocation[Raw]): Unit
  def call(invocation: RawInvocation[Raw]): Future[Raw]
  def get(invocation: RawInvocation[Raw]): RawRpc[Raw]
