package mrpc.codec

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import mrpc.codec.LeafCodecFixture.{roundTrip, User}

class LeafCodecProps extends munit.ScalaCheckSuite:

  given Arbitrary[User] = Arbitrary:
    for
      id <- Gen.choose(Int.MinValue, Int.MaxValue)
      name <- Gen.alphaNumStr
    yield User(id, name)

  property("leaf DTO round-trips"):
    forAll: (u: User) =>
      roundTrip(u) == u
