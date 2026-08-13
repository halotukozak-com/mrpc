package halotukozak.mrpc.transport

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import halotukozak.mrpc.conv.{AsRaw, AsReal}
import halotukozak.mrpc.derive.SampleApi.*
import halotukozak.mrpc.raw.RawRpc

/**
 * The full real->raw->transport->raw->real round-trip over the sample trait: the bare loopback passes
 * the `RawRpc[String]` directly, whereas here a single concrete [[InMemoryTransport]] sits in the
 * middle, so every `fire`/`call`/`get` crosses an explicit transport boundary before reaching the
 * server adapter. This is ROADMAP criterion 2 — one in-memory `RawRpc[String]` instantiation exercised
 * end-to-end (HTTP/WebSocket out of scope).
 *
 * Because [[InMemoryTransport.get]] re-wraps the sub-RPC in a fresh transport, the recursion cases
 * re-cross the boundary at every nesting level, not just the top call.
 */
class InMemoryTransportSuite extends munit.FunSuite:

  // Leaf JSON codec givens + parasitic EC must be in scope where both directions materialize.
  import halotukozak.mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  private var pinged: Boolean = false

  // A concrete real implementation; `pinged` lets the fire round-trip observe the side effect.
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

  // The non-recursive sub-RPC conversions both directions route through (the get seam summons these).
  private given AsRaw[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsRaw[UsersRpc]
  private given AsReal[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsReal[UsersRpc]

  // real -> raw (server adapter) -> TRANSPORT -> raw -> real (client proxy): the round-trip under test.
  private val server: RawRpc[String] = SampleApiCodec.materializeAsRaw[SampleApi].asRaw(impl)
  private val transported: RawRpc[String] = new InMemoryTransport(server)
  private val proxy: SampleApi = SampleApiCodec.materializeAsReal[SampleApi].asReal(transported)

  test("a full real->raw->transport->raw->real call returns the direct result"):
    assertEquals(await(proxy.increment(41)), 42)
    assertEquals(await(proxy.combine(1)("x", 2L)), "1-x-2")

  test("a fire op crosses the transport and reaches the real impl"):
    pinged = false
    proxy.ping()
    assert(pinged)

  test("a sub-RPC getter crosses the transport boundary and returns the right value"):
    assertEquals(await(proxy.users.count()), 7)

  // --- Self-referential recursion through the transport -------------------------------------------
  // The recursive adapter/proxy are held in lazy vals read back through makeLazy so nested summons
  // resolve to the placeholder instead of re-deriving; the transport re-wraps each get, so every
  // nesting level re-crosses the boundary.
  private lazy val selfRaw: AsRaw[RawRpc[String], SelfRpc] =
    AsRaw.makeLazy(SampleApiCodec.materializeAsRaw[SelfRpc])
  private lazy val selfReal: AsReal[RawRpc[String], SelfRpc] =
    AsReal.makeLazy(SampleApiCodec.materializeAsReal[SelfRpc])
  private given AsRaw[RawRpc[String], SelfRpc] = selfRaw
  private given AsReal[RawRpc[String], SelfRpc] = selfReal

  test("a self-referential recursion round-trips through the transport"):
    val selfServer: RawRpc[String] = selfRaw.asRaw(selfImpl)
    val selfTransported: RawRpc[String] = new InMemoryTransport(selfServer)
    val selfProxy: SelfRpc = selfReal.asReal(selfTransported)
    assertEquals(await(selfProxy.value()), 1)
    // child crosses a fresh transport (get re-wrapped) to the deeper level; leafSelf.value() == 99.
    assertEquals(await(selfProxy.child.value()), 99)
