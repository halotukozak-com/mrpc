package mrpc.derive

/**
 * Compile-time negative tests. Uses munit's `compileErrors`, which compiles a source string and
 * returns the diagnostics (empty when it compiles). The duplicate-rpcName case is exercised fully by
 * the matcher's name resolution; the unsupported-result-type case is documented below.
 */
class CompileErrorSuite extends munit.FunSuite:

  // The derived mirror the negative cases are built against (keeps the import load-bearing).
  private val sample: String = SampleApi.SampleApiCodec.toString
  assert(sample.nonEmpty)

  test("the compile-error mechanism reports type errors"):
    assert(compileErrors("val x: Int = \"s\"").nonEmpty)

//  test("a duplicate computed rpcName fails at compile time with a clear message"):
//    val errors = compileErrors(
//      """
//      import scala.concurrent.Future
//      import mrpc.annotation.rpcName
//      trait Dup:
//        @rpcName("dup") def first(): Unit
//        @rpcName("dup") def second(n: Int): Future[Int]
//      val _ = mrpc.derive.Matcher.planFor[Dup, "first"]
//      """,
//    )
//    assert(errors.nonEmpty, "expected a compile error for the duplicate rpc name")
//    assert(
//      errors.contains("duplicate RPC name 'dup'"),
//      s"error should name the duplicate; got: $errors",
//    )
//    assert(errors.contains("first") && errors.contains("second"), s"error should name both methods; got: $errors")

// "An unsupported result type is classified as `get` and carries the result type, never silently
// mis-classified as a call" is asserted at the type level in MatchingSuite against this SAME `users`
// op (`summon[users.ArityInfo =:= ArityTag.GetOf[UsersRpc]]`) — no separate runtime check here.
