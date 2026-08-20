package halotukozak.mrpc.conv

/** Combines [[AsRaw]] and [[AsReal]] into a single bidirectional conversion. */
trait AsRawReal[Raw, Real] extends AsReal[Raw, Real], AsRaw[Raw, Real]

object AsRawReal extends AsRawRealLowPrio:
  def apply[Raw, Real](using instance: AsRawReal[Raw, Real]): AsRawReal[Raw, Real] = instance

  def create[Raw, Real](asRawFun: Real => Raw, asRealFun: Raw => Real): AsRawReal[Raw, Real] =
    new AsRawReal[Raw, Real]:
      def asRaw(real: Real): Raw = asRawFun(real)
      def asReal(raw: Raw): Real = asRealFun(raw)

  // Deliberately diverges from commons' reusable-identity cast trick to satisfy the no-cast style
  // rule; we build a fresh instance instead, since the per-call allocation cost is negligible.
  given identity[A]: AsRawReal[A, A] with
    def asRaw(real: A): A = real
    def asReal(raw: A): A = raw

/** Low-priority instances so `fromSeparate` does not collide with `identity` when `Raw = Real`. */
private[conv] trait AsRawRealLowPrio extends AsRawRealLowestPrio:
  given fromSeparate[Raw, Real](
    using
    asRaw: AsRaw[Raw, Real],
    asReal: AsReal[Raw, Real],
  ): AsRawReal[Raw, Real] = AsRawReal.create(asRaw.asRaw, asReal.asReal)

/** Lowest-priority instance, below `fromSeparate` too — `Fallback` is meant as a last resort. */
private[conv] trait AsRawRealLowestPrio:
  given fromFallback[Raw, Real](using fallback: halotukozak.mrpc.Fallback[AsRawReal[Raw, Real]]): AsRawReal[Raw, Real] =
    fallback.value
