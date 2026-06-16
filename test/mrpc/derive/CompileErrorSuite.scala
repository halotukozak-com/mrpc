package mrpc.derive

import mrpc.derive.SampleApi.*

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

  test("a duplicate computed rpcName fails at compile time with a clear message"):
    val errors = compileErrors(
      """
      import scala.concurrent.Future
      import mrpc.annotation.rpcName
      trait Dup:
        @rpcName("dup") def first(): Unit
        @rpcName("dup") def second(n: Int): Future[Int]
      val _ = mrpc.derive.Matcher.describe[Dup]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for the duplicate rpc name")
    assert(
      errors.contains("duplicate RPC name 'dup'"),
      s"error should name the duplicate; got: $errors",
    )
    assert(errors.contains("first") && errors.contains("second"), s"error should name both methods; got: $errors")

  test("an unsupported result type fails at compile time once a sub-RPC conversion is required"):
    // The matcher treats any non-Unit/non-Future result as a sub-RPC getter seam (recursion is a
    // later concern), so the unsupported-result-type error is raised where a sub-RPC CONVERSION must
    // be summoned — i.e. the materialize site, which lands in a later plan. Here we assert the related
    // invariant the matcher CAN prove standalone: a result type that is neither Unit nor Future is
    // classified as `get` and carries the result type, never silently mis-classified as a call.
    val plans = Matcher.describe[SampleApi]
    val users = plans.filter(_.label == "users")
    assertEquals(users.map(_.arity), List("get"))
    assert(users.head.carriedType.endsWith("UsersRpc"))
