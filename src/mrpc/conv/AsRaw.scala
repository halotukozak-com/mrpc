package halotukozak.mrpc.conv

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** Converts a real value into its raw representation. Generic over an abstract `Raw` type. */
trait AsRaw[Raw, Real]:
  def asRaw(real: Real): Raw

object AsRaw:
  def apply[Raw, Real](using instance: AsRaw[Raw, Real]): AsRaw[Raw, Real] = instance

  given identity[A]: AsRaw[A, A] = (a: A) => a

  // Typeclass instance OVER Try (not a Try-returning public method) — required for commons parity.
  given forTry[Raw, Real](using inner: AsRaw[Raw, Real]): AsRaw[Try[Raw], Try[Real]] =
    _.map(inner.asRaw)

  // ExecutionContext is taken via `using` rather than captured globally; the call site supplies it.
  given forFuture[Raw, Real](
    using
    inner: AsRaw[Raw, Real],
    ec: ExecutionContext,
  ): AsRaw[Future[Raw], Future[Real]] =
    _.map(inner.asRaw)

  /**
   * By-name wrapper that defers construction of the underlying conversion until first use. Breaks
   * self/mutually-referential sub-RPC cycles: the recursive adapter is forced lazily at runtime, not
   * re-derived during macro expansion. Mirrors mcodec MCodec.makeLazy.
   */
  def makeLazy[Raw, Real](conv: => AsRaw[Raw, Real]): AsRaw[Raw, Real] = new AsRaw[Raw, Real]:
    private lazy val c = conv
    def asRaw(real: Real): Raw = c.asRaw(real)
