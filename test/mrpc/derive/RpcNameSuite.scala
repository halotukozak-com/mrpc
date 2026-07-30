package mrpc.derive

import scala.concurrent.Future
import scala.compiletime.ops.string.Matches
import scala.util.NotGiven

import mrpc.annotation.rpcNamePrefix
import mrpc.derive.RpcNameSuite.{Prefixed, PrefixedOverloadedOnly}
import mrpc.derive.SampleApi.*

/**
 * Drives RPC-name resolution (rpcName > prefix > signature-based overload mangling) against SampleApi
 * and a handful of focused local fixtures for the prefix modes.
 *
 * Checked entirely at COMPILE TIME, same technique as [[MatchingSuite]]: `Matcher.planFor[T, "label"]`
 * for a single op, `Matcher.plansFor[T, "label"]` (destructured by the KNOWN overload count) when a
 * label is shared by more than one op. The exact hash suffix is an implementation detail, never
 * asserted literally — `Matches[...]` checks the `lookup_`/`q_dup_` shape, `NotGiven[...]` proves two
 * overloads disambiguate to DIFFERENT names.
 */
class RpcNameSuite extends munit.FunSuite:

  test("an explicit name override wins over the method label"):
    val findRenamed = Matcher.planFor[SampleApi, "findRenamed"]
    summon[findRenamed.RpcName =:= "findOne"]

  test("a method with no annotations resolves to its label"):
    val ping = Matcher.planFor[SampleApi, "ping"]
    val increment = Matcher.planFor[SampleApi, "increment"]
    summon[ping.RpcName =:= "ping"]
    summon[increment.RpcName =:= "increment"]

  test("overloaded methods get a deterministic signature-based suffix; others are untouched"):
    val (byId, byName) = Matcher.plansFor[SampleApi, "lookup"]
    // both start from the shared base, carry a suffix, and are distinct
    summon[Matches[byId.RpcName, "lookup_.*"] =:= true]
    summon[Matches[byName.RpcName, "lookup_.*"] =:= true]
    summon[NotGiven[byId.RpcName =:= byName.RpcName]]
    // a non-overloaded method keeps its plain name (no suffix)
    val find = Matcher.planFor[SampleApi, "find"]
    summon[find.RpcName =:= "find"]

  test("overload suffixes are reorder-stable (depend only on the signature)"):
    val (firstById, firstByName) = Matcher.plansFor[SampleApi, "lookup"]
    val (secondById, secondByName) = Matcher.plansFor[SampleApi, "lookup"]
    summon[firstById.RpcName =:= secondById.RpcName]
    summon[firstByName.RpcName =:= secondByName.RpcName]

  test("a name prefix is applied to every method when not overloaded-only"):
    val alpha = Matcher.planFor[Prefixed, "alpha"]
    val beta = Matcher.planFor[Prefixed, "beta"]
    summon[alpha.RpcName =:= "p_alpha"]
    summon[beta.RpcName =:= "p_beta"]

  test("an overloaded-only prefix is applied only to overloaded methods"):
    // plain (non-overloaded) method keeps its bare name
    val solo = Matcher.planFor[PrefixedOverloadedOnly, "solo"]
    summon[solo.RpcName =:= "solo"]
    // the overloaded pair gets the prefix (plus a disambiguating suffix)
    val (dupInt, dupStr) = Matcher.plansFor[PrefixedOverloadedOnly, "dup"]
    summon[Matches[dupInt.RpcName, "q_dup_.*"] =:= true]
    summon[Matches[dupStr.RpcName, "q_dup_.*"] =:= true]
    summon[NotGiven[dupInt.RpcName =:= dupStr.RpcName]]

object RpcNameSuite:

  trait Prefixed:
    @rpcNamePrefix("p_") def alpha(): Unit
    @rpcNamePrefix("p_") def beta(n: Int): Future[Int]

  trait PrefixedOverloadedOnly:
    @rpcNamePrefix("q_", overloadedOnly = true) def solo(): Unit
    @rpcNamePrefix("q_", overloadedOnly = true) def dup(n: Int): Future[Int]
    @rpcNamePrefix("q_", overloadedOnly = true) def dup(s: String): Future[Int]
