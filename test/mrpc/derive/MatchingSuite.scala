package mrpc.derive

import made.Done

import mrpc.derive.SampleApi.*

/**
 * Wave 0 scaffold for the standalone matcher (op -> OpPlan). Bodies are pending until the matcher
 * lands; they name their target so the matcher work has concrete, automated checks from line one.
 */
class MatchingSuite extends munit.FunSuite:

  // The derived mirror these pending tests will drive the matcher against.
  private val sample: Done.Of[SampleApi] = Done.derived[SampleApi]

  test("arity is classified from the output type (Unit->fire, Future[X]->call, sub-RPC->get)".ignore):
    // TODO: filled by the matcher — assert each SampleApi op maps to its expected arity.
    val _ = sample

  test("each value param gets an encode-vs-verbatim plan".ignore):
    // TODO: filled by the matcher — assert default-encode under abstract Raw, verbatim only when
    // the param type is Raw itself.
    ()

  test("rpcName routing carries the computed name onto the op plan".ignore):
    // TODO: filled by the matcher — assert the resolved rpcName lands in the plan.
    ()

  test("default encoding follows arity semantics".ignore):
    // TODO: filled by the matcher — assert default-encoding-by-arity matches the documented rules.
    ()
