package mrpc.raw

import mrpc.conv.{AsRaw, AsRawReal, AsReal}

/**
 * User-facing entry point a transport chooses a concrete `Raw` for. Exposes type aliases over the
 * chosen `Raw`, `asReal`/`asRaw` helpers delegating to summoned conversion instances, and the three
 * derivation entry points.
 */
trait RawRpcCompanion[Raw]:
  type AsRawRpc[Real] = AsRaw[Raw, Real]
  type AsRealRpc[Real] = AsReal[Raw, Real]
  type AsRawRealRpc[Real] = AsRawReal[Raw, Real]

  def asRawRpc[Real](using a: AsRawRpc[Real]): AsRawRpc[Real] = a
  def asRealRpc[Real](using a: AsRealRpc[Real]): AsRealRpc[Real] = a
  def asRawRealRpc[Real](using a: AsRawRealRpc[Real]): AsRawRealRpc[Real] = a

  def asReal[Real](raw: Raw)(using a: AsRealRpc[Real]): Real = a.asReal(raw)
  def asRaw[Real](real: Real)(using a: AsRawRpc[Real]): Raw = a.asRaw(real)

  // Derivation entry points — real bodies land in the derivation phase. compiletime.error fires
  // only if called, so the API type-checks now and premature use is a compile error.
  inline def materializeAsRaw[Real]: AsRawRpc[Real] =
    scala.compiletime.error("materializeAsRaw is not implemented yet")
  inline def materializeAsReal[Real]: AsRealRpc[Real] =
    scala.compiletime.error("materializeAsReal is not implemented yet")
  inline def materializeAsRawReal[Real]: AsRawRealRpc[Real] =
    scala.compiletime.error("materializeAsRawReal is not implemented yet")
