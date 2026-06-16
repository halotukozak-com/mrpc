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

  // The server adapter: a `Real` trait yields a `RawRpc[Raw]`, so its conversion is
  // `AsRaw[RawRpc[Raw], Real]` (distinct from the leaf `AsRawRpc[Real] = AsRaw[Raw, Real]` codec).
  inline def materializeAsRaw[Real]: AsRaw[RawRpc[Raw], Real] =
    ${ mrpc.derive.AsRawDerivation.impl[Raw, Real] }

  // The client proxy: a `RawRpc[Raw]` yields a `Real` trait implementation, so its conversion is
  // `AsReal[RawRpc[Raw], Real]` (distinct from the leaf `AsRealRpc[Real] = AsReal[Raw, Real]` codec).
  inline def materializeAsReal[Real]: AsReal[RawRpc[Raw], Real] =
    ${ mrpc.derive.AsRealDerivation.impl[Raw, Real] }

  // The combined direction is not needed by any consumer yet; it can be assembled from the two
  // separate directions when a use case appears. compiletime.error fires only if called, so the API
  // type-checks now and premature use is a compile error.
  inline def materializeAsRawReal[Real]: AsRawRealRpc[Real] =
    scala.compiletime.error("materializeAsRawReal is not implemented yet")
