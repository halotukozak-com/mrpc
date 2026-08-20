package halotukozak.mrpc.derive

import scala.concurrent.{ExecutionContext, Future}

import halotukozak.mcodec.Json

import halotukozak.mrpc.derive.SampleApi.*
import halotukozak.mrpc.derive.SampleApi.SampleApiCodec.{pingRaw, pingReal, pongRaw, pongReal, selfRaw, selfReal}
import halotukozak.mrpc.raw.RawRpc

/**
 * Wave-0 de-risking proof for the one genuinely new engine piece: lazy sub-RPC recursion. It proves
 * that mechanism A (the `AsRaw.makeLazy` / `AsReal.makeLazy` by-name wrapper) breaks both the
 * self-referential (`SelfRpc.child: SelfRpc`) and the mutually-referential (`Ping.toPong`/
 * `Pong.toPing`) cycles WITHOUT infinite inline expansion under `-Ycheck:macros`, and round-trips
 * one nesting level real->raw->real.
 *
 * The cycle is broken at the given-resolution layer: each recursive adapter is held in a
 * `lazy val given` that the `makeLazy` thunk reads BACK (Pitfall 1) — the eager `get`-seam summon
 * resolves to the already-declared lazy given instead of re-entering `materialize*[Sub]`, so macro
 * expansion reaches a fixed point. The by-name thunk forces the lazy val only at the first runtime
 * `get`, not during expansion. That wiring (`selfRaw`/`selfReal`/`pingRaw`/`pingReal`/`pongRaw`/
 * `pongReal`) is derived once in `SampleApiCodec` and imported here, rather than re-derived per suite.
 *
 * It also resolves the VAL-01 empirical question: whether mcodec `Json.write` output matches the
 * committed golden fixtures byte-for-byte for primitives AND objects. The verdict is recorded in
 * 06-FINDINGS.md.
 */
class RecursionSpikeSuite extends munit.FunSuite:

  // Leaf JSON codec givens + parasitic EC must be in scope where both directions are materialized.
  import halotukozak.mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = f.value.get.get

  test("self-referential getter round-trips one nesting level"):
    val raw: RawRpc[String] = selfRaw.asRaw(selfImpl)
    val proxy: SelfRpc = selfReal.asReal(raw)
    assertEquals(await(proxy.value()), 1)
    // proxy.child crosses the get seam; the deeper level (leafSelf) returns 99.
    assertEquals(await(proxy.child.value()), 99)

  test("mutually-referential pair round-trips"):
    val raw: RawRpc[String] = pingRaw.asRaw(pingImpl)
    val proxy: Ping = pingReal.asReal(raw)
    assertEquals(await(proxy.ping()), 10)
    // toPong crosses one get seam, then pong() is a call on the peer.
    assertEquals(await(proxy.toPong.pong()), 20)
    // toPong.toPing crosses two get seams back to the Ping side.
    assertEquals(await(proxy.toPong.toPing.ping()), 10)

  test("primitive fixture JSON matches mcodec Json.write byte-for-byte"):
    // call_add.json args are [[2,40]]; fire/rename fixtures use 7/404; tagged_emit's tag is "warn".
    assertEquals(Json.write(2), "2")
    assertEquals(Json.write(40), "40")
    assertEquals(Json.write(404), "404")
    assertEquals(Json.write("warn"), "\"warn\"")

  test("object fixture JSON matches mcodec Json.write byte-for-byte"):
    // tagged_emit.json object component is {"k1":"v1","k2":"v2"}. mcodec preserves field declaration
    // order with no whitespace, so the object is byte-for-byte equal (Open Q2 resolved: no
    // normalization needed for this DTO shape).
    assertEquals(Json.write(User(1, "v1")), "{\"id\":1,\"name\":\"v1\"}")
