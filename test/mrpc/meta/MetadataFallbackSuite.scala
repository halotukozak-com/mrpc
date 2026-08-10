package mrpc.meta

import mrpc.Fallback
import mrpc.derive.SampleApi.SampleApi

import scala.annotation.unused

/**
 * DIVERGENCES.md D17: `RpcMetadataCompanion.fromFallback` mirrors commons `MetadataCompanion.fromFallback` —
 * a `Fallback[M[Real]]` resolves via implicit search when no normal `given M[Real]` is in scope, but a
 * normal given still wins over it (no ambiguity). This class needs no steering annotations: neither test
 * calls `.materialize`, so the constructor param is never read by the metadata macro.
 */
final case class SimpleMeta[T](tag: String)
object SimpleMeta extends RpcMetadataCompanion[SimpleMeta]

class MetadataFallbackSuite extends munit.FunSuite:

  test("a Fallback-wrapped metadata instance resolves via implicit search when nothing else provides one"):
    given Fallback[SimpleMeta[SampleApi]] = Fallback(SimpleMeta("from-fallback"))
    assertEquals(summon[SimpleMeta[SampleApi]].tag, "from-fallback")

  test("a normal given wins over a Fallback-wrapped competitor, no ambiguity"):
    // Present but never actually resolved — that's exactly the point being asserted below.
    @unused given Fallback[SimpleMeta[SampleApi]] = Fallback(SimpleMeta("from-fallback"))
    given SimpleMeta[SampleApi] = SimpleMeta("from-normal")
    assertEquals(summon[SimpleMeta[SampleApi]].tag, "from-normal")
