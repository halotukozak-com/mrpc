package halotukozak.mrpc.derive

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import halotukozak.mrpc.derive.SampleApi.*
import halotukozak.mrpc.derive.SampleApi.SampleApiCodec.{pingRaw, pingReal, pongRaw, pongReal, selfRaw, selfReal}
import halotukozak.mrpc.raw.RawRpc

/**
 * The production-API analog of `RecursionSpikeSuite`: it round-trips self- and mutually-referential
 * RPC traits real->raw->real, proving the now-lazy `get`-arity seams (both derivations route through
 * `AsRaw.makeLazy`/`AsReal.makeLazy`) break the recursion cycle WITHOUT infinite inline expansion
 * under `-Ycheck:macros`. This is the behavioural test ENG-06 closes on; the spike stays as the
 * mechanism-discovery compile proof.
 *
 * The cycle is broken at the given-resolution layer (the FINDINGS Pitfall-1 placement): each recursive
 * adapter/proxy is held in a `lazy val given` the seam's eager summon reads BACK through `makeLazy`,
 * so nested `materialize*[Sub]` resolves to the already-declared lazy given instead of re-deriving,
 * and the by-name thunk forces the underlying conversion only at the first runtime `get`. That wiring
 * (`selfRaw`/`selfReal`/`pingRaw`/`pingReal`/`pongRaw`/`pongReal`) is derived once in `SampleApiCodec`
 * and imported here, rather than re-derived per suite.
 */
class RecursionSuite extends munit.FunSuite:

  // Leaf JSON codec givens + parasitic EC must be in scope where both directions materialize.
  import halotukozak.mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  test("a self-referential getter round-trips real->raw->real one nesting level"):
    val raw: RawRpc[String] = selfRaw.asRaw(selfImpl)
    val proxy: SelfRpc = selfReal.asReal(raw)
    assertEquals(await(proxy.value()), 1)
    // proxy.child crosses the now-lazy get seam to the deeper level; leafSelf.value() returns 99.
    assertEquals(await(proxy.child.value()), 99)
    // A second nesting level (child.child) must not blow the inline/implicit depth limit at compile
    // time and still resolves to the terminating leaf at runtime.
    assertEquals(await(proxy.child.child.value()), 99)

  test("a mutually-referential pair round-trips through alternating getters"):
    val raw: RawRpc[String] = pingRaw.asRaw(pingImpl)
    val proxy: Ping = pingReal.asReal(raw)
    assertEquals(await(proxy.ping()), 10)
    // toPong crosses one get seam, then pong() is a call on the peer.
    assertEquals(await(proxy.toPong.pong()), 20)
    // toPong.toPing crosses two get seams back to the Ping side.
    assertEquals(await(proxy.toPong.toPing.ping()), 10)
