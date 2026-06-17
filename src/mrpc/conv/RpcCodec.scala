package mrpc.conv

/**
 * Opt-in derivation marker — mrpc's analog of commons' `extends RPCCompanion[T]`. Writing
 *
 * {{{ trait MyApi derives RpcCodec: ... }}}
 *
 * places a `given RpcCodec[MyApi]` in `MyApi`'s companion. That marker gates the auto-derivation
 * givens [[AsRaw.derivedRpc]] / [[AsReal.derivedRpc]], so `summon[AsRaw[RawRpc[Raw], MyApi]]` and
 * `summon[AsReal[RawRpc[Raw], MyApi]]` resolve WITHOUT any manual `materializeAsRaw`/`materializeAsReal`
 * call. The marker carries no data; it only signals "this trait is an RPC interface, derive for it".
 *
 * Gating (rather than a blanket given for every `Real`) keeps the engine from trying to derive an
 * RPC adapter for arbitrary, non-RPC types.
 */
sealed trait RpcCodec[Real]

object RpcCodec:
  /** Entry point for the `derives RpcCodec` clause. A fresh, data-free marker per opted-in trait. */
  def derived[Real]: RpcCodec[Real] = new RpcCodec[Real] {}
