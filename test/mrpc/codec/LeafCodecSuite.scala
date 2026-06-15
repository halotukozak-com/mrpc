package mrpc.codec

import mcodec.MCodec
import mrpc.conv.{AsRaw, AsReal}
import mrpc.codec.JsonRawValue.given

/** Shared fixture for the leaf-codec round-trip tests. */
object LeafCodecFixture:
  case class User(id: Int, name: String) derives MCodec

  /** Encodes a real value to the raw JSON form and back via the MCodec-derived bridge. */
  def roundTrip[A: MCodec](a: A): A =
    AsReal[String, A].asReal(AsRaw[String, A].asRaw(a))

class LeafCodecSuite extends munit.FunSuite:
  import LeafCodecFixture.*

  test("Int round-trips through the MCodec leaf bridge"):
    assertEquals(roundTrip(42), 42)

  test("String round-trips through the MCodec leaf bridge"):
    assertEquals(roundTrip("hello"), "hello")

  test("Boolean round-trips through the MCodec leaf bridge"):
    assertEquals(roundTrip(true), true)

  test("DTO round-trips through the MCodec leaf bridge"):
    assertEquals(roundTrip(User(1, "a")), User(1, "a"))
