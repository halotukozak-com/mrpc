package halotukozak.mrpc.conv

import halotukozak.mrpc.Fallback

import scala.annotation.unused

/** `Fallback[T]` lowers an implicit's priority below normal givens (D17). */
class FallbackSuite extends munit.FunSuite:

  // Distinct Raw/Real so `identity` can never apply and only `fromFallback` is a candidate.
  final case class Wrapped(s: String)

  test("AsRaw: a Fallback-wrapped instance resolves when no normal given is in scope"):
    given Fallback[AsRaw[String, Wrapped]] = Fallback((w: Wrapped) => w.s)
    assertEquals(summon[AsRaw[String, Wrapped]].asRaw(Wrapped("x")), "x")

  test("AsRaw: a normal given wins over a Fallback-wrapped competitor, no ambiguity"):
    @unused given Fallback[AsRaw[String, Wrapped]] = Fallback((_: Wrapped) => "from-fallback")
    given AsRaw[String, Wrapped] = (w: Wrapped) => "from-normal:" + w.s
    assertEquals(summon[AsRaw[String, Wrapped]].asRaw(Wrapped("x")), "from-normal:x")

  test("AsReal: a Fallback-wrapped instance resolves when no normal given is in scope"):
    given Fallback[AsReal[String, Wrapped]] = Fallback((s: String) => Wrapped(s))
    assertEquals(summon[AsReal[String, Wrapped]].asReal("x"), Wrapped("x"))

  test("AsReal: a normal given wins over a Fallback-wrapped competitor, no ambiguity"):
    @unused given Fallback[AsReal[String, Wrapped]] = Fallback((_: String) => Wrapped("from-fallback"))
    given AsReal[String, Wrapped] = (s: String) => Wrapped("from-normal:" + s)
    assertEquals(summon[AsReal[String, Wrapped]].asReal("x"), Wrapped("from-normal:x"))

  test("AsRawReal: a Fallback-wrapped instance resolves when no normal given/fromSeparate applies"):
    given Fallback[AsRawReal[String, Wrapped]] = Fallback(AsRawReal.create[String, Wrapped](_.s, Wrapped(_)))
    val instance = summon[AsRawReal[String, Wrapped]]
    assertEquals(instance.asRaw(Wrapped("x")), "x")
    assertEquals(instance.asReal("x"), Wrapped("x"))

  test("AsRawReal: fromSeparate (built from AsRaw+AsReal givens) wins over a Fallback competitor"):
    @unused given Fallback[AsRawReal[String, Wrapped]] =
      Fallback(AsRawReal.create[String, Wrapped](_ => "from-fallback", _ => Wrapped("from-fallback")))
    given AsRaw[String, Wrapped] = (w: Wrapped) => "from-separate:" + w.s
    given AsReal[String, Wrapped] = (s: String) => Wrapped("from-separate:" + s)
    val instance = summon[AsRawReal[String, Wrapped]]
    assertEquals(instance.asRaw(Wrapped("x")), "from-separate:x")
    assertEquals(instance.asReal("x"), Wrapped("from-separate:x"))
