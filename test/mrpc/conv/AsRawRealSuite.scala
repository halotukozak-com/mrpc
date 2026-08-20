package halotukozak.mrpc.conv

class AsRawRealSuite extends munit.FunSuite:
  test("identity AsRawReal resolves without ambiguity and round-trips"):
    val instance = summon[AsRawReal[Int, Int]]
    assertEquals(instance.asRaw(2), 2)
    assertEquals(instance.asReal(2), 2)

  test("create builds an AsRawReal from a pair of functions that round-trips"):
    val instance = AsRawReal.create[String, Int](_.toString, _.toInt)
    assertEquals(instance.asReal(instance.asRaw(5)), 5)

  test("fromSeparate combines an AsRaw and an AsReal in scope"):
    given AsRaw[String, Int] = (real: Int) => real.toString
    given AsReal[String, Int] = (raw: String) => raw.toInt
    val instance = summon[AsRawReal[String, Int]]
    assertEquals(instance.asRaw(7), "7")
    assertEquals(instance.asReal("7"), 7)
