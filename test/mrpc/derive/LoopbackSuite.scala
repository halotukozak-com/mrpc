package halotukozak.mrpc.derive

import scala.concurrent.{ExecutionContext, Future}

import halotukozak.mrpc.derive.SampleApi.*
import halotukozak.mrpc.derive.SampleApi.SampleApiCodec.{selfRaw, selfReal}
import halotukozak.mrpc.raw.RawRpc
import halotukozak.mrpc.transport.InMemoryTransport

/**
 * The headline real->raw->real loopback over the sample trait: materialize a real `SampleApi` impl
 * to a `RawRpc[String]` server adapter, materialize that raw rpc back to a `SampleApi` CLIENT PROXY,
 * then drive typed calls on the proxy and assert the values match the original impl's behaviour. This
 * exercises the full round-trip for the `call`, `fire`, and (non-recursive) sub-RPC `get` arities.
 */
class LoopbackSuite extends munit.FunSuite:

  // The leaf JSON codec givens and a parasitic ExecutionContext must be in scope where both
  // directions are materialized (the abstract-Raw summon proof established this placement).
  import halotukozak.mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private var pinged: Boolean = false

  // A concrete real implementation. `pinged` lets the fire round-trip observe the side effect.
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

  // The non-recursive sub-RPC conversion (get/recursion seams) and the self-referential wiring
  // (`selfRaw`/`selfReal`, imported above) are derived once in `SampleApiCodec` and shared here.

  // real -> raw (server adapter) -> real (client proxy): the full loopback under test.
  private val rawRpc: RawRpc[String] = SampleApiCodec.sampleApiRaw.asRaw(impl)
  private val proxy: SampleApi = SampleApiCodec.sampleApiReal.asReal(rawRpc)

  private def await[A](f: Future[A]): A = f.value.get.get

  test("a call op round-trips real->raw->real and returns the right value"):
    assertEquals(await(proxy.increment(41)), 42)
    assertEquals(await(proxy.find(7)), User(7, "user-7"))

  test("a multi-param-list call op round-trips through nested args"):
    assertEquals(await(proxy.combine(1)("x", 2L)), "1-x-2")

  test("a primitive/wrapper-returning call op round-trips without a verify error"):
    assertEquals(await(proxy.echoBool(true)), true)
    assertEquals(await(proxy.echoBool(false)), false)

  test("a fire op routes through fire and reaches the real impl"):
    pinged = false
    proxy.ping()
    assert(pinged)

  test("a sub-RPC getter round-trips real->raw->real through the get->call path"):
    val users: UsersRpc = proxy.users
    assertEquals(await(users.count()), 7)

  test("a self-referential getter round-trips real->raw->real"):
    val selfRpc: RawRpc[String] = selfRaw.asRaw(selfImpl)
    val selfProxy: SelfRpc = selfReal.asReal(selfRpc)
    assertEquals(await(selfProxy.value()), 1)
    // child crosses the now-lazy get seam to the fixed leaf level (leafSelf.value() == 99).
    assertEquals(await(selfProxy.child.value()), 99)

  test("the full stack round-trips through an in-memory transport hop"):
    // real -> raw -> TRANSPORT -> raw -> real: the loopback with InMemoryTransport sandwiched in.
    val transported: RawRpc[String] = new InMemoryTransport(SampleApiCodec.sampleApiRaw.asRaw(impl))
    val tProxy: SampleApi = SampleApiCodec.sampleApiReal.asReal(transported)
    assertEquals(await(tProxy.increment(41)), 42)
    // a sub-RPC getter crosses the transport boundary (re-wrapped) before reaching the impl.
    assertEquals(await(tProxy.users.count()), 7)
