package halotukozak.mrpc.meta

import halotukozak.mrpc.Fallback
import halotukozak.mrpc.derive.SampleApi.SampleApi

import scala.annotation.unused

// No steering annotations needed: neither test calls .materialize.
final case class SimpleMeta[T](tag: String)
object SimpleMeta extends RpcMetadataCompanion[SimpleMeta]

class MetadataFallbackSuite extends munit.FunSuite:

  test("a Fallback-wrapped metadata instance resolves via implicit search when nothing else provides one"):
    given Fallback[SimpleMeta[SampleApi]] = Fallback(SimpleMeta("from-fallback"))
    assertEquals(summon[SimpleMeta[SampleApi]].tag, "from-fallback")

  test("a normal given wins over a Fallback-wrapped competitor, no ambiguity"):
    @unused given Fallback[SimpleMeta[SampleApi]] = Fallback(SimpleMeta("from-fallback"))
    given SimpleMeta[SampleApi] = SimpleMeta("from-normal")
    assertEquals(summon[SimpleMeta[SampleApi]].tag, "from-normal")
