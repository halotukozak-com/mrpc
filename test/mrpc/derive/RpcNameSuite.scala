package mrpc.derive

import made.Done

import mrpc.derive.SampleApi.*

/**
 * Wave 0 scaffold for RPC-name resolution (rpcName > prefix > overload mangling). Bodies are pending
 * until the matcher lands.
 */
class RpcNameSuite extends munit.FunSuite:

  // The derived mirror these pending tests will resolve names against.
  private val sample: Done.Of[SampleApi] = Done.derived[SampleApi]

  test("an explicit name override wins over the method label".ignore):
    // TODO: filled by the matcher — assert findRenamed resolves to "findOne".
    val _ = sample

  test("a name prefix is applied (always, or only to overloaded methods)".ignore):
    // TODO: filled by the matcher — assert prefix application under both modes.
    ()

  test("overloaded methods get a deterministic signature-based suffix; others are untouched".ignore):
    // TODO: filled by the matcher — assert the lookup overloads disambiguate while ping stays plain.
    ()
