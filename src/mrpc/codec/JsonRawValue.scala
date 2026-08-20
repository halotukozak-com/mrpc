package halotukozak.mrpc.codec

import halotukozak.mcodec.{Json, MCodec}
import halotukozak.mrpc.conv.{AsRaw, AsReal}

/**
 * Concrete JSON-string instantiation of the leaf codec bridge. The generic engine keeps `Raw`
 * abstract; this object is the test/transport seam where `Raw = String` (JSON). Mirrors commons'
 * codec-based encoding living in a raw-RPC companion's implicits object: a leaf `AsRaw`/`AsReal`
 * pair derived from the value codec, here `halotukozak.mcodec.MCodec` standing in for `GenCodec`.
 */
object JsonRawValue:
  // A leaf value encodes into the raw (JSON String) form via its MCodec.
  given mcodecAsRaw[A: MCodec]: AsRaw[String, A] = (a: A) => Json.write(a)

  // A raw (JSON String) decodes back to the exact declared type via its MCodec.
  given mcodecAsReal[A: MCodec]: AsReal[String, A] = (s: String) => Json.read[A](s)
