package mrpc.derive

import scala.concurrent.Future

import mrpc.annotation.rpcNamePrefix
import mrpc.derive.RpcNameSuite.{Prefixed, PrefixedOverloadedOnly}
import mrpc.derive.SampleApi.*

/**
 * Drives RPC-name resolution (rpcName > prefix > signature-based overload mangling) against SampleApi
 * and a handful of focused local fixtures for the prefix modes.
 */
class RpcNameSuite extends munit.FunSuite:

  private val plans: List[OpDescriptor] = Matcher.describe[SampleApi]
  private def names(label: String): List[String] = plans.filter(_.label == label).map(_.rpcName)

  test("an explicit name override wins over the method label"):
    assertEquals(names("findRenamed"), List("findOne"))

  test("a method with no annotations resolves to its label"):
    assertEquals(names("ping"), List("ping"))
    assertEquals(names("increment"), List("increment"))

  test("overloaded methods get a deterministic signature-based suffix; others are untouched"):
    val lookupNames = plans.filter(_.label == "lookup").map(_.rpcName)
    assertEquals(lookupNames.size, 2)
    // both start from the shared base, carry a suffix, and are distinct
    assert(lookupNames.forall(_.startsWith("lookup_")), s"expected suffixed names, got $lookupNames")
    assertEquals(lookupNames.distinct.size, 2, s"overloads must disambiguate, got $lookupNames")
    // a non-overloaded method keeps its plain name (no suffix)
    assertEquals(names("find"), List("find"))

  test("overload suffixes are reorder-stable (depend only on the signature)"):
    val first = Matcher.describe[SampleApi].filter(_.label == "lookup").map(_.rpcName)
    val second = Matcher.describe[SampleApi].filter(_.label == "lookup").map(_.rpcName)
    assertEquals(first, second)

  test("a name prefix is applied to every method when not overloaded-only"):
    assertEquals(Matcher.describe[Prefixed].map(_.rpcName).sorted, List("p_alpha", "p_beta"))

  test("an overloaded-only prefix is applied only to overloaded methods"):
    val resolved = Matcher.describe[PrefixedOverloadedOnly]
    // plain (non-overloaded) method keeps its bare name
    assertEquals(resolved.filter(_.label == "solo").map(_.rpcName), List("solo"))
    // the overloaded pair gets the prefix (plus a disambiguating suffix)
    val dup = resolved.filter(_.label == "dup").map(_.rpcName)
    assertEquals(dup.size, 2)
    assert(dup.forall(_.startsWith("q_dup_")), s"expected prefixed+suffixed overloads, got $dup")
    assertEquals(dup.distinct.size, 2)

object RpcNameSuite:

  trait Prefixed:
    @rpcNamePrefix("p_") def alpha(): Unit
    @rpcNamePrefix("p_") def beta(n: Int): Future[Int]

  trait PrefixedOverloadedOnly:
    @rpcNamePrefix("q_", overloadedOnly = true) def solo(): Unit
    @rpcNamePrefix("q_", overloadedOnly = true) def dup(n: Int): Future[Int]
    @rpcNamePrefix("q_", overloadedOnly = true) def dup(s: String): Future[Int]
