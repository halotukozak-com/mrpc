package mrpc.derive

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import mrpc.conv.{AsRaw, AsReal}
import mrpc.derive.SampleApi.*
import mrpc.raw.RawRpc

/**
 * The production-API analog of `RecursionSpikeSuite`: it round-trips self- and mutually-referential
 * RPC traits real->raw->real through the public `SampleApiCodec.materializeAsRaw`/`materializeAsReal`
 * entry points, proving the now-lazy `get`-arity seams (both derivations route through
 * `AsRaw.makeLazy`/`AsReal.makeLazy`) break the recursion cycle WITHOUT infinite inline expansion
 * under `-Ycheck:macros`. This is the behavioural test ENG-06 closes on; the spike stays as the
 * mechanism-discovery compile proof.
 *
 * The cycle is broken at the given-resolution layer (the FINDINGS Pitfall-1 placement): each recursive
 * adapter/proxy is held in a `lazy val given` the seam's eager summon reads BACK through `makeLazy`,
 * so nested `materialize*[Sub]` resolves to the already-declared lazy given instead of re-deriving,
 * and the by-name thunk forces the underlying conversion only at the first runtime `get`.
 */
class RecursionSuite extends munit.FunSuite:

  // Leaf JSON codec givens + parasitic EC must be in scope where both directions materialize.
  import mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  // --- Self-referential wiring (lazy val given read back through makeLazy) -------------------------
  // SelfRpc.child returns SelfRpc; the lazy val is the placeholder the seam's nested summon resolves
  // to, so materialize*[SelfRpc] reaches a fixed point instead of expanding forever.
  private lazy val selfRaw: AsRaw[RawRpc[String], SelfRpc] =
    AsRaw.makeLazy(SampleApiCodec.materializeAsRaw[SelfRpc])
  private lazy val selfReal: AsReal[RawRpc[String], SelfRpc] =
    AsReal.makeLazy(SampleApiCodec.materializeAsReal[SelfRpc])
  private given AsRaw[RawRpc[String], SelfRpc] = selfRaw
  private given AsReal[RawRpc[String], SelfRpc] = selfReal

  // --- Mutually-referential wiring (Ping.toPong -> Pong, Pong.toPing -> Ping) ----------------------
  // Each side's adapter/proxy needs its peer's; both are lazy givens read back through makeLazy so the
  // Ping<->Pong cycle resolves the peer to the lazy placeholder rather than re-deriving it.
  private lazy val pingRaw: AsRaw[RawRpc[String], Ping] =
    AsRaw.makeLazy(SampleApiCodec.materializeAsRaw[Ping])
  private lazy val pingReal: AsReal[RawRpc[String], Ping] =
    AsReal.makeLazy(SampleApiCodec.materializeAsReal[Ping])
  private lazy val pongRaw: AsRaw[RawRpc[String], Pong] =
    AsRaw.makeLazy(SampleApiCodec.materializeAsRaw[Pong])
  private lazy val pongReal: AsReal[RawRpc[String], Pong] =
    AsReal.makeLazy(SampleApiCodec.materializeAsReal[Pong])
  private given AsRaw[RawRpc[String], Ping] = pingRaw
  private given AsReal[RawRpc[String], Ping] = pingReal
  private given AsRaw[RawRpc[String], Pong] = pongRaw
  private given AsReal[RawRpc[String], Pong] = pongReal

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
