package mrpc.derive

import made.Done

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import mcodec.Json
import mrpc.conv.AsRaw
import mrpc.derive.SampleApi.*
import mrpc.raw.{RawInvocation, RawRpc}

/**
 * Decode-then-invoke dispatch safety: primitive and wrapper args must round-trip through
 * decode -> Done.invoke without unsafe-cast crashes (ClassCastException / VerifyError). The server
 * adapter is materialized for the sample trait at `Raw = String` (JSON), then driven with crafted
 * raw invocations.
 */
class DispatchSafetySuite extends munit.FunSuite:

  // The leaf JSON codec givens and a parasitic ExecutionContext must be in scope where the server
  // adapter is materialized (the abstract-Raw summon proof established this placement).
  import mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  // A concrete real implementation the server adapter dispatches against.
  private val impl: SampleApi = new SampleApi:
    def ping(): Unit = ()
    def increment(n: Int): Future[Int] = Future.successful(n + 1)
    def find(id: Int): Future[User] = Future.successful(User(id, s"user-$id"))
    def users: UsersRpc = new UsersRpc:
      def count(): Future[Int] = Future.successful(7)
    def lookup(id: Int): Future[User] = Future.successful(User(id, "by-id"))
    def lookup(name: String): Future[User] = Future.successful(User(-1, name))
    def combine(a: Int)(b: String, c: Long): Future[String] = Future.successful(s"$a-$b-$c")
    def echoBool(b: Boolean): Future[Boolean] = Future.successful(b)
    def findRenamed(id: Int): Future[User] = Future.successful(User(id, "renamed"))

  // The non-recursive sub-RPC adapter the `users` get arm routes to (the recursion seam summons it).
  private given AsRaw[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsRaw[UsersRpc]

  // The materialized server adapter under test.
  private val rawRpc: RawRpc[String] =
    SampleApiCodec.materializeAsRaw[SampleApi].asRaw(impl)

  private def raw[A: mcodec.MCodec](a: A): String = Json.write(a)
  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  test("an Int arg round-trips through decode->invoke"):
    val result = await(rawRpc.call(RawInvocation("increment", List(List(raw(41))))))
    assertEquals(Json.read[Int](result), 42)

  test("a Boolean arg round-trips through decode->invoke"):
    val resultTrue = await(rawRpc.call(RawInvocation("echoBool", List(List(raw(true))))))
    assertEquals(Json.read[Boolean](resultTrue), true)
    val resultFalse = await(rawRpc.call(RawInvocation("echoBool", List(List(raw(false))))))
    assertEquals(Json.read[Boolean](resultFalse), false)

  test("a DTO result round-trips through decode->invoke"):
    val result = await(rawRpc.call(RawInvocation("find", List(List(raw(5))))))
    assertEquals(Json.read[User](result), User(5, "user-5"))

  test("multi-param-list args rebuild via the per-param-list arities"):
    // combine(a: Int)(b: String, c: Long): the nested args mirror the two parameter lists.
    val inv = RawInvocation("combine", List(List(raw(1)), List(raw("x"), raw(2L))))
    val result = await(rawRpc.call(inv))
    assertEquals(Json.read[String](result), "1-x-2")

  test("a fire op dispatches without a result"):
    // ping() returns Unit; the only observable effect is that it does not throw.
    rawRpc.fire(RawInvocation("ping", Nil))

  test("an unknown rpc name is rejected, not silently accepted"):
    intercept[IllegalArgumentException]:
      await(rawRpc.call(RawInvocation("doesNotExist", Nil)))

  test("an unknown rpc name on fire is rejected too, not a silent no-op"):
    intercept[IllegalArgumentException]:
      rawRpc.fire(RawInvocation("doesNotExist", Nil))
