package halotukozak.mrpc.raw

class RawInvocationSuite extends munit.FunSuite:

  test("constructs with all three fields and exposes them"):
    val inv = RawInvocation[String](
      rpcName = "add",
      args = List(List("2", "40")),
      metadata = Map("trace" -> "abc"),
    )
    assertEquals(inv.rpcName, "add")
    assertEquals(inv.args, List(List("2", "40")))
    assertEquals(inv.metadata, Map("trace" -> "abc"))

  test("two-arg construction defaults metadata to empty"):
    val inv = RawInvocation[String]("ping", List(List("7")))
    assertEquals(inv.metadata, Map.empty[String, String])

  test("copy preserves args while changing rpcName"):
    val inv = RawInvocation[String]("add", List(List("1", "2")))
    val renamed = inv.copy(rpcName = "sum")
    assertEquals(renamed.rpcName, "sum")
    assertEquals(renamed.args, inv.args)

  test("value equality of two equal invocations"):
    val a = RawInvocation[String]("add", List(List("1", "2")), Map("k" -> "v"))
    val b = RawInvocation[String]("add", List(List("1", "2")), Map("k" -> "v"))
    assertEquals(a, b)
