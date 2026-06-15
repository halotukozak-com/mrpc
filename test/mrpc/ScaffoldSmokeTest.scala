package mrpc

class ScaffoldSmokeTest extends munit.FunSuite:
  test("scaffold toolchain compiles and runs tests"):
    assertEquals(scaffoldOk, true)
