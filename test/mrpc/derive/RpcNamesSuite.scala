package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * The whole-trait [[RpcNames]] derivation must resolve the SAME names as the engine's
 * [[Matcher.describe]] / [[RpcName.computeAll]] — over the REAL `SampleApi`, whose ops carry
 * `MetaAnnotation`s (`@multi`, `@rpcName`, …). This is the annotation-proof path that the per-op
 * `summonAll` cannot take.
 */
class RpcNamesSuite extends munit.FunSuite:

  test("RpcNames.Names resolves all rpcNames over an annotated trait"):
    val rn = summon[RpcNames[SampleApi]]
    val resolved: List[String] = rn.names.toList.map(_.toString)
    assertEquals(resolved.sorted, Matcher.describe[SampleApi].map(_.rpcName).sorted)
