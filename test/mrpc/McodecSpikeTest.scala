package mrpc

import mcodec.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll

/**
 * Capability spike: confirms mcodec leaf codecs cover the four areas the RPC engine will lean on:
 *   1. primitives + a DTO/case class,
 *   2. value-class / `@transparent` wrappers (serialized as the bare value),
 *   3. `@multi` collections (List / Map / Option),
 *   4. a property-based round-trip over a generated DTO.
 *
 * Effect wrappers (Future / Try) are deliberately NOT exercised here: mcodec has no leaf codec for
 * them and that is correct — effect-wrapper handling is an engine-layer concern, not a codec gap.
 * See MCODEC_SPIKE_NOTES.md.
 */
object McodecSpikeTest:
  case class User(id: Int, name: String) derives MCodec

  @made.annotation.transparent
  case class Email(value: String) derives MCodec

  given Arbitrary[User] = Arbitrary:
    for
      id <- Gen.choose(Int.MinValue, Int.MaxValue)
      name <- Gen.alphaNumStr
    yield User(id, name)

class McodecSpikeTest extends munit.ScalaCheckSuite:
  import McodecSpikeTest.*

  test("primitives round-trip through JSON"):
    assertEquals(Json.read[Int](Json.write(42)), 42)
    assertEquals(Json.read[String](Json.write("hello")), "hello")
    assertEquals(Json.read[Boolean](Json.write(true)), true)

  test("DTO / case class round-trips through JSON"):
    val user = User(1, "a")
    assertEquals(Json.read[User](Json.write(user)), user)

  test("@transparent wrapper serializes as the bare value and round-trips"):
    // No wrapper object in the JSON — Email serializes exactly as its underlying String.
    assertEquals(Json.write(Email("a@b")), "\"a@b\"")
    assertEquals(Json.read[Email](Json.write(Email("a@b"))), Email("a@b"))

  test("@multi collections round-trip: List / Map / Option"):
    assertEquals(Json.read[List[Int]](Json.write(List(1, 2, 3))), List(1, 2, 3))
    assertEquals(Json.read[Map[String, Int]](Json.write(Map("a" -> 1))), Map("a" -> 1))
    assertEquals(Json.read[Option[Int]](Json.write(Option(7))), Option(7))
    assertEquals(Json.read[Option[Int]](Json.write(Option.empty[Int])), Option.empty[Int])

  property("generated DTO round-trips real -> raw -> real"):
    forAll: (u: User) =>
      Json.read[User](Json.write(u)) == u
