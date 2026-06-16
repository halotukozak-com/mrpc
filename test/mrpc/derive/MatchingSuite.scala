package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * Drives the standalone matcher (op -> OpPlan) against SampleApi: arity classification from the
 * output type and the per-param encode-vs-verbatim plan under abstract `Raw`.
 */
class MatchingSuite extends munit.FunSuite:

  private val plans: List[OpDescriptor] = Matcher.describe[SampleApi]
  private def byLabel(label: String): List[OpDescriptor] = plans.filter(_.label == label)

  test("arity is classified from the output type (Unit->fire, Future[X]->call, sub-RPC->get)"):
    assertEquals(byLabel("ping").map(_.arity), List("fire"))
    assertEquals(byLabel("increment").map(_.arity), List("call"))
    assertEquals(byLabel("find").map(_.arity), List("call"))
    assertEquals(byLabel("users").map(_.arity), List("get"))

  test("call arity carries its result type; get arity carries the sub-RPC type"):
    assertEquals(byLabel("increment").map(_.carriedType), List("scala.Int"))
    assert(byLabel("find").head.carriedType.endsWith("User"))
    assert(byLabel("users").head.carriedType.endsWith("UsersRpc"))

  test("each value param gets an encode-vs-verbatim plan (every value param encoded under abstract Raw)"):
    // increment(n: Int) -> one encoded param; echoBool(b: Boolean) -> one encoded param.
    assertEquals(byLabel("increment").head.paramEncodings, List("encoded"))
    assertEquals(byLabel("echoBool").head.paramEncodings, List("encoded"))
    // combine(a: Int)(b: String, c: Long) flattens to three encoded params.
    assertEquals(byLabel("combine").head.paramEncodings, List("encoded", "encoded", "encoded"))
    // ping() has no value params.
    assertEquals(byLabel("ping").head.paramEncodings, Nil)

  test("default result encoding is encoded under abstract Raw"):
    assertEquals(byLabel("find").head.resultEncoding, "encoded")
    assertEquals(byLabel("increment").head.resultEncoding, "encoded")
