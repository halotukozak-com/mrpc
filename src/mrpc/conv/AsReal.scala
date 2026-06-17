package mrpc.conv

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** Converts a raw value back into its real representation. Mirror of [[AsRaw]]. */
trait AsReal[Raw, Real]:
  def asReal(raw: Raw): Real

object AsReal:
  def apply[Raw, Real](using instance: AsReal[Raw, Real]): AsReal[Raw, Real] = instance

  given identity[A]: AsReal[A, A] = (a: A) => a

  /**
   * Auto-derived client proxy for any RPC trait opted in via `derives RpcCodec` (commons-style — no
   * manual `materializeAsReal`). Gated on the [[RpcCodec]] marker so it never fires for non-RPC types;
   * polymorphic in the transport `Raw` and `inline` so leaf codecs resolve at the concrete summon site.
   */
  inline given derivedRpc[Raw, Real](using RpcCodec[Real]): AsReal[mrpc.raw.RawRpc[Raw], Real] =
    ${ mrpc.derive.AsRealDerivation.impl[Raw, Real] }

  // Typeclass instance OVER Try (not a Try-returning public method) — required for commons parity.
  given forTry[Raw, Real](using inner: AsReal[Raw, Real]): AsReal[Try[Raw], Try[Real]] =
    _.map(inner.asReal)

  // ExecutionContext is taken via `using` rather than captured globally; the call site supplies it.
  given forFuture[Raw, Real](
    using
    inner: AsReal[Raw, Real],
    ec: ExecutionContext,
  ): AsReal[Future[Raw], Future[Real]] =
    _.map(inner.asReal)

  /**
   * By-name wrapper that defers construction of the underlying conversion until first use. Breaks
   * self/mutually-referential sub-RPC cycles: the recursive adapter is forced lazily at runtime, not
   * re-derived during macro expansion. Mirrors mcodec MCodec.makeLazy.
   */
  def makeLazy[Raw, Real](conv: => AsReal[Raw, Real]): AsReal[Raw, Real] = new AsReal[Raw, Real]:
    private lazy val c = conv
    def asReal(raw: Raw): Real = c.asReal(raw)
