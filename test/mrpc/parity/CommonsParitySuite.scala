package mrpc.parity

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import mrpc.conv.{AsRaw, AsReal}
import mrpc.derive.SampleApi.*
import mrpc.derive.SampleApi
import mrpc.raw.RawRpc

/**
 * A representative subset of the AVSystem/commons RPC test suite, ported as BEHAVIOUR (not source).
 * Commons is Scala 2 + ScalaTest + GenCodec and cannot run in mrpc's process, so the test INTENT is
 * re-expressed against mrpc's API, reusing the SampleApi loopback machinery and the recursion wiring
 * (the lazy-val-given read back through `AsRaw.makeLazy`/`AsReal.makeLazy`).
 *
 * Mapped from these commons sources:
 *   - RPCTest / TestRPC / InnerRPC: fire/call/get dispatch + the get-then-call sub-RPC chain, plus
 *     `InnerRPC.moreInner` (self-recursion) and `indirectRecursion` (mutual recursion).
 *   - MangleOverloadsTest: overload disambiguation (commons asserts `one_1`/`two_2`).
 *   - NewRawRpc (rpcName/prefix intent only): `@rpcName` override + `@multi`/arity classification.
 *
 * SKIPPED here, with the deliberate-divergence rationale (see DIVERGENCES.md):
 *   - The exact overload-suffix strings (`one_1`/`two_2`): mrpc uses a reorder-stable signature-hash
 *     suffix, NOT commons' positional `_1`/`_2` (D4). We assert distinct routing, not the strings.
 *   - `@optional`/`@whenAbsent`/`@multi`-collection extraction (D7): arity/default chain is v2.
 *   - `@composite`, generic methods (`generallyDoStuff[T]`), varargs, by-name, interceptors, and the
 *     generic user-declared `RawRpc` machinery of `NewRawRpc` (D8): mrpc's fixed three-method RawRpc
 *     deliberately does not implement these; SKIP and document rather than port.
 */
class CommonsParitySuite extends munit.FunSuite:

  // Leaf JSON codec givens + a parasitic ExecutionContext, in scope where both directions materialize.
  import mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  // --- The real SampleApi impl driven through the loopback (commons RPCTest.rpcImpl analog) --------
  private var pinged: Boolean = false
  private val impl: SampleApi = new SampleApi:
    def ping(): Unit = pinged = true
    def increment(n: Int): Future[Int] = Future.successful(n + 1)
    def find(id: Int): Future[User] = Future.successful(User(id, s"user-$id"))
    def users: UsersRpc = new UsersRpc:
      def count(): Future[Int] = Future.successful(7)
    def lookup(id: Int): Future[User] = Future.successful(User(id, "by-id"))
    def lookup(name: String): Future[User] = Future.successful(User(-1, name))
    def combine(a: Int)(b: String, c: Long): Future[String] = Future.successful(s"$a-$b-$c")
    def echoBool(b: Boolean): Future[Boolean] = Future.successful(b)
    def findRenamed(id: Int): Future[User] = Future.successful(User(id, "renamed"))

  // The non-recursive sub-RPC conversions the get seam summons (commons binds these implicitly too).
  private given AsRaw[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsRaw[UsersRpc]
  private given AsReal[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsReal[UsersRpc]

  // real -> raw (server adapter) -> real (client proxy): the loopback the dispatch tests drive.
  private val rawRpc: RawRpc[String] = SampleApiCodec.materializeAsRaw[SampleApi].asRaw(impl)
  private val proxy: SampleApi = SampleApiCodec.materializeAsReal[SampleApi].asReal(rawRpc)

  // --- Recursion wiring (lazy-val givens read back through makeLazy) -------------------------------
  // Self-referential (InnerRPC.moreInner analog) and mutually-referential (indirectRecursion analog).
  private lazy val selfRaw: AsRaw[RawRpc[String], SelfRpc] =
    AsRaw.makeLazy(SampleApiCodec.materializeAsRaw[SelfRpc])
  private lazy val selfReal: AsReal[RawRpc[String], SelfRpc] =
    AsReal.makeLazy(SampleApiCodec.materializeAsReal[SelfRpc])
  private given AsRaw[RawRpc[String], SelfRpc] = selfRaw
  private given AsReal[RawRpc[String], SelfRpc] = selfReal

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

  // --- 1. fire/call/get dispatch (commons RPCTest intent) ------------------------------------------

  test("fire dispatch reaches the impl (RPCTest fire(handleMore) analog)"):
    pinged = false
    proxy.ping()
    assert(pinged)

  test("call dispatch returns the typed result (RPCTest call(doStuffBoolean) analog)"):
    assertEquals(await(proxy.increment(41)), 42)
    assertEquals(await(proxy.find(7)), User(7, "user-7"))

  test("get dispatch yields a sub-RPC whose call returns the right value (RPCTest get(innerRpc).call analog)"):
    val users: UsersRpc = proxy.users
    assertEquals(await(users.count()), 7)

  // --- 2. Recursion chain (commons InnerRPC.moreInner self + indirectRecursion mutual) -------------

  test("self-referential getter chain round-trips (moreInner analog)"):
    val raw: RawRpc[String] = selfRaw.asRaw(selfImpl)
    val self: SelfRpc = selfReal.asReal(raw)
    assertEquals(await(self.value()), 1)
    // child crosses the lazy get seam to the deeper level; child.child must not blow the inline depth.
    assertEquals(await(self.child.value()), 99)
    assertEquals(await(self.child.child.value()), 99)

  test("mutually-referential getter chain round-trips (indirectRecursion analog)"):
    val raw: RawRpc[String] = pingRaw.asRaw(pingImpl)
    val ping: Ping = pingReal.asReal(raw)
    assertEquals(await(ping.ping()), 10)
    // toPong crosses one get seam to the peer; toPong.toPing crosses two back to the Ping side.
    assertEquals(await(ping.toPong.pong()), 20)
    assertEquals(await(ping.toPong.toPing.ping()), 10)

  // --- 3. Overload disambiguation (commons MangleOverloadsTest intent) -----------------------------

  test("each overload routes distinctly with no collision (MangleOverloadsTest one/two analog)"):
    // commons asserts the positional suffix strings (one_1, two_2). mrpc uses a reorder-stable
    // signature-hash suffix (D4 in DIVERGENCES.md), so we assert the BEHAVIOUR — that each overload
    // routes to its own distinct handler — not the exact suffix strings.
    val byId: User = await(proxy.lookup(7))
    val byName: User = await(proxy.lookup("alice"))
    assertEquals(byId, User(7, "by-id"))
    assertEquals(byName, User(-1, "alice"))
    assertNotEquals(byId, byName)
