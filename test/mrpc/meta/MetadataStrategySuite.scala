package mrpc.meta

import mrpc.derive.SampleApi.*

// RED until strategy-marker honoring lands; see phase plan 03.
class MetadataStrategySuite extends munit.FunSuite:
  object MetaFixture extends RpcMetadataCompanionV1

  private lazy val md: RpcMetadata[SampleApi] = MetaFixture.materializeMetadata[SampleApi]

  test("@rpcMethodMetadata projection: operations is one entry per RPC method"):
    // distinct methods (overloads counted separately) -> 9 ops for SampleApi
    assertEquals(md.operations.size, 9)

  test("@rpcParamMetadata projection: params is the per-param, declaration-order projection"):
    val combine = md.operations.find(_.label == "combine").get
    assertEquals(combine.params.map(_.name), List("a", "b", "c"))

  test("the materializer RECOGNIZES the strategy markers (does not ignore them)"):
    // Plan 03 adds RpcMetadata.recognizedStrategies: the two markers the materializer honors.
    assertEquals(
      RpcMetadata.recognizedStrategies,
      Set("rpcMethodMetadata", "rpcParamMetadata"),
    )
