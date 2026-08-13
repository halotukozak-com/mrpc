package halotukozak.mrpc.raw

import halotukozak.mrpc.conv.{AsRaw, AsReal}

class RawRpcCompanionSuite extends munit.FunSuite:

  // A concrete companion proves RawRpcCompanion is usable as an entry point for a chosen Raw.
  object Ex extends RawRpcCompanion[String]

  given AsRaw[String, Int] = (i: Int) => i.toString
  given AsReal[String, Int] = (s: String) => s.toInt

  test("asRaw delegates to a summoned AsRaw instance"):
    assertEquals(Ex.asRaw[Int](42), "42")

  test("asReal delegates to a summoned AsReal instance"):
    assertEquals(Ex.asReal[Int]("42"), 42)

  test("type aliases resolve to the in-scope givens"):
    val raw = summon[Ex.AsRawRpc[Int]]
    val real = summon[Ex.AsRealRpc[Int]]
    assertEquals(raw.asRaw(7), "7")
    assertEquals(real.asReal("7"), 7)

  test("materializeAsRaw rejects a non-RPC Real at compile time"):
    // `Int` has no Done mirror / RPC shape, so deriving a server adapter for it must fail to compile
    // rather than silently produce a broken instance.
    assert(compileErrors("Ex.materializeAsRaw[Int]").nonEmpty)
