package halotukozak.mrpc.meta

import halotukozak.made.Made
import halotukozak.mrpc.derive.SampleApi.*

/**
 * The migrated v1 `MetadataStrategySuite`, rewritten onto the TypedMetadata DSL. The v1 surface
 * `recognizedStrategies` is retired; recognition is now PROVEN by the projections genuinely producing
 * the right collections (the macro honors `@rpcMethodMetadata`/`@rpcParamMetadata`, it does not ignore
 * them). Reuses the `ApiInfo`/`MethodInfo`/`ParamInfo` fixture from `MetadataSuite`.
 */
class MetadataStrategySuite extends munit.FunSuite:

  private lazy val md: ApiInfo[SampleApi] = ApiInfo.materialize[SampleApi]

  test("@rpcMethodMetadata @multi projection: one entry per RPC method (10 ops)"):
    // distinct methods (overloads counted separately) -> 10 ops for SampleApi
    assertEquals(md.methods.size, 10)

  test("@rpcParamMetadata @multi projection: params is the per-param, declaration-order projection"):
    val combine = md.methods.find(_.label == "combine").get
    assertEquals(combine.params.map(_.name), List("a", "b", "c"))

  test("the materializer RECOGNIZES the strategy markers (fills, does not ignore)"):
    // Recognition is observable: every method has a materialized per-param projection, and the param
    // collection is non-empty exactly when the method declares params.
    assert(md.methods.nonEmpty)
    val increment = md.methods.find(_.label == "increment").get
    assertEquals(increment.params.map(_.name), List("n"))
    val ping = md.methods.find(_.label == "ping").get
    assertEquals(ping.params, Nil)
