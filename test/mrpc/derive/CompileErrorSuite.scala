package mrpc.derive

import made.Done

import mrpc.derive.SampleApi.*

/**
 * Wave 0 scaffold for compile-time negative tests. One active smoke assertion exercises the
 * compile-error mechanism now; the real negative cases are pending until the matcher and macros land.
 */
class CompileErrorSuite extends munit.FunSuite:

  // The derived mirror the pending negative cases will be built against.
  private val sample: Done.Of[SampleApi] = Done.derived[SampleApi]

  test("the compile-error mechanism reports type errors"):
    assert(compileErrors("val x: Int = \"s\"").nonEmpty)

  test("a duplicate computed rpcName fails at compile time with a clear message".ignore):
    // TODO: filled by the matcher/macros — assert the collision is reported, naming both methods.
    val _ = sample

  test("an unmatched op (unsupported result type) fails at compile time".ignore):
    // TODO: filled by the matcher/macros — assert the unsupported-output-type error is reported.
    ()
