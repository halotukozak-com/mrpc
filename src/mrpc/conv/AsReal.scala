package mrpc.conv

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** Converts a raw value back into its real representation. Mirror of [[AsRaw]]. */
trait AsReal[Raw, Real]:
  def asReal(raw: Raw): Real

object AsReal:
  def apply[Raw, Real](using instance: AsReal[Raw, Real]): AsReal[Raw, Real] = instance

  given identity[A]: AsReal[A, A] = (a: A) => a

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
