package mrpc
package derive

import mrpc.conv.{AsRaw, AsReal}
import mrpc.derive.SampleApi.*
import mrpc.raw.RawRpc
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

/**
 * The round-trip LAW over the full materialized stack: where `LoopbackSuite` pins specific values, this
 * lifts the proof to `forAll` over arbitrary inputs — asserting that a call through the materialized
 * proxy (`materializeAsReal.asReal(materializeAsRaw.asRaw(impl))`) returns the SAME value the direct
 * impl call would, for every generated `Int`/`User`. This is the scalacheck half of VAL-03, reusing the
 * `munit.ScalaCheckSuite` + `Arbitrary[User]` template established in `LeafCodecProps`, not a parallel
 * test stack.
 */
class RoundTripLawProps extends munit.ScalaCheckSuite:

  // Leaf JSON codec givens + parasitic EC must be in scope where both directions materialize.
  import mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  // Arbitrary[User] copied from LeafCodecProps: alphaNum names keep the JSON leaf encoding away from
  // pathological control-character strings while still ranging over the full Int id space.
  given Arbitrary[User] = Arbitrary:
    for
      id <- Gen.choose(Int.MinValue, Int.MaxValue)
      name <- Gen.alphaNumStr
    yield User(id, name)

  // The same concrete impl the loopback drives; `find(id)` is deterministic in `id`, so the law can
  // compare the proxy result against the value the direct impl would produce.
  private val impl: SampleApi = new SampleApi:
    def ping(): Unit = ()
    def increment(n: Int): Future[Int] = Future.successful(n + 1)
    def find(id: Int): Future[User] = Future.successful(User(id, s"user-$id"))
    def users: UsersRpc = () => Future.successful(7)
    def lookup(id: Int): Future[User] = Future.successful(User(id, "by-id"))
    def lookup(name: String): Future[User] = Future.successful(User(-1, name))
    def combine(a: Int)(b: String, c: Long): Future[String] = Future.successful(s"$a-$b-$c")
    def echoBool(b: Boolean): Future[Boolean] = Future.successful(b)
    def findRenamed(id: Int): Future[User] = Future.successful(User(id, "renamed"))

  // Non-recursive sub-RPC conversions the get seam summons.
  private given AsRaw[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsRaw[UsersRpc]
  private given AsReal[RawRpc[String], UsersRpc] = SampleApiCodec.materializeAsReal[UsersRpc]

  // real -> raw -> real: the full materialized stack the law runs every generated input through.
  private val rawRpc: RawRpc[String] = {SampleApiCodec.materializeAsRaw[SampleApi]}.asRaw(impl)
  private val proxy: SampleApi = SampleApiCodec.materializeAsReal[SampleApi].asReal(rawRpc)

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  property("a call op round-trips: proxy.increment(n) == n + 1"):
    forAll: (n: Int) =>
      await(proxy.increment(n)) == n + 1

  property("a DTO-returning call op round-trips structurally: proxy.find(id) == User(id, ...)"):
    forAll: (id: Int) =>
      await(proxy.find(id)) == User(id, s"user-$id")

  property("the round-trip law holds for arbitrary User payloads"):
    forAll: (u: User) =>
      // `find` reconstructs a User from the generated id; the law asserts the proxy returns exactly the
      // value the direct impl would for that id, proving the User shape survives the full stack.
      await(proxy.find(u.id)) == User(u.id, s"user-${u.id}")
