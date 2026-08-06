package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * The whole-trait [[RpcNames]] derivation resolves every op's rpcName over the REAL `SampleApi`, whose
 * ops carry `MetaAnnotation`s (`@multi`, `@rpcName`, …) — the annotation-proof path that the per-op
 * `summonAll` cannot take. `Matcher`/`MetadataDerivation` both source their names from THIS SAME
 * derivation ([[RpcNames.namesOf]]), so there is nothing else to cross-check against.
 */
class RpcNamesSuite extends munit.FunSuite:

  test("RpcNames.Names resolves all rpcNames over an annotated trait"):
    val rn = RpcNames.materialize[SampleApi]
    val resolved: List[String] = compiletime.constValueTuple[rn.Underlying].toList.map(_.toString)
    assertEquals(resolved.size, 9)
    // every op except the overloaded `lookup` pair resolves to its plain (unsuffixed) name
    val plain = Set("ping", "increment", "find", "users", "combine", "echoBool", "findOne")
    assertEquals(resolved.filterNot(_.startsWith("lookup_")).toSet, plain)
    // the overloaded pair disambiguates to two distinct signature-hash-suffixed names
    val lookupNames = resolved.filter(_.startsWith("lookup_"))
    assertEquals(lookupNames.size, 2)
    assertEquals(lookupNames.distinct.size, 2)
