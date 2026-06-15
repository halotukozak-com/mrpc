package mrpc.conv

import org.scalacheck.Prop.forAll

class ConvRoundTripProps extends munit.ScalaCheckSuite:
  property("identity round-trips Int values: asReal(asRaw(n)) == n"):
    forAll { (n: Int) =>
      AsReal[Int, Int].asReal(AsRaw[Int, Int].asRaw(n)) == n
    }

  property("identity round-trips String values: asReal(asRaw(s)) == s"):
    forAll { (s: String) =>
      AsReal[String, String].asReal(AsRaw[String, String].asRaw(s)) == s
    }
