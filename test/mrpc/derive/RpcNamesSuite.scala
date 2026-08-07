package mrpc
package derive

import made.*
import mrpc.derive.SampleApi.*

/**
 * The whole-trait [[RpcNames]] derivation resolves every op's rpcName over the REAL `SampleApi`, whose
 * ops carry `MetaAnnotation`s (`@multi`, `@rpcName`, …) — the annotation-proof path that the per-op
 * `summonAll` cannot take. `Matcher`/`MetadataDerivation` both source their names from THIS SAME
 * derivation ([[RpcNames.namesOf]]), so there is nothing else to cross-check against.
 */
class RpcNamesSuite extends munit.FunSuite:
  import RpcNames.*

  test("RpcNames.Names resolves all rpcNames over an annotated trait"):
    ???
//    val done = Done.derived[SampleApi]
//    val rn: RpcNames.Proxy[mrpc.derive.SampleApi.SampleApi] {
//      type Underlying = (
//        "ping_501",
//        "increment_2cd4d40a",
//        "find_2cd4d40a",
//        "users_501",
//        "lookup_2cd4d40a",
//        "lookup_5753eb3c",
//        "combine_4d23242b",
//        "echoBool_27ed0ef1",
//        "findOne",
//      )
//    } = RpcNames.materialize2[SampleApi]
//    val resolved: List[String] = compiletime.constValueTuple[rn.Underlying].toList.map(_.toString)
//    assertEquals(resolved.size, 9)
//    // every op except the overloaded `lookup` pair resolves to its plain (unsuffixed) name
//    val plain = Set("ping", "increment", "find", "users", "combine", "echoBool", "findOne")
//    assertEquals(resolved.filterNot(_.startsWith("lookup_")).toSet, plain)
//    // the overloaded pair disambiguates to two distinct signature-hash-suffixed names
//    val lookupNames = resolved.filter(_.startsWith("lookup_"))
//    assertEquals(lookupNames.size, 2)
//    assertEquals(lookupNames.distinct.size, 2)
