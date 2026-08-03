package mrpc.meta

import made.Made
import mrpc.annotation.{multi, reifyAnnot, reifyName, rpcMethodMetadata, rpcParamMetadata}
import mrpc.derive.SampleApi.{SampleApi, *}

// Per-param metadata for the migrated v1 suite: source name + the optional @multi annotation a param
// may carry (SampleApi.increment's `n` is @multi).
final case class ParamInfo[T](
  @reifyName name: String,
  @reifyAnnot multiAnnot: Option[mrpc.annotation.multi],
) extends TypedMetadata[T]

// Per-method metadata: source label, resolved rpcName, the optional @rpcName instance, and the
// per-param projection in declaration order.
final case class MethodInfo[T](
  @reifyName label: String,
  @reifyName(useRawName = true) rpcName: String,
  @reifyAnnot rpcNameAnnot: Option[mrpc.annotation.rpcName],
  @rpcParamMetadata @multi params: List[ParamInfo[?]],
) extends TypedMetadata[T]

// Trait-level metadata: one MethodInfo per RPC method.
final case class ApiInfo[T](
  @rpcMethodMetadata @multi methods: List[MethodInfo[?]],
)
object ApiInfo extends RpcMetadataCompanion[ApiInfo]

/**
 * The migrated v1 `MetadataSuite`, rewritten onto the TypedMetadata DSL (the v1 flat `RpcMetadata`
 * surface is retired). Re-asserts the v1 behaviors against the new shape — every op listed, params in
 * declaration order, resolved rpcName, per-method/per-param annotation. The no-fork invariant (resolved
 * rpcNames == the engine's) no longer needs its own check: both this metadata path and the engine
 * source rpcNames from the SAME `RpcNames.namesOf` authority (see `MetadataDerivation`/`Matcher`), so
 * they cannot diverge by construction. Arity classification is asserted at the type level in
 * `mrpc.derive.MatchingSuite` (the DSL itself has no arity slot — there is one classifier).
 */
class MetadataSuite extends munit.FunSuite:

  private lazy val md: ApiInfo[SampleApi] = ApiInfo.materialize[SampleApi]
  private def op(label: String): MethodInfo[?] = md.methods.find(_.label == label).get

  test("methods lists every op of the trait"):
    assertEquals(
      md.methods.map(_.label).toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed"),
    )

  test("params are listed in declaration order, flattened across param lists"):
    assertEquals(op("combine").params.map(_.name), List("a", "b", "c"))
    assertEquals(op("ping").params.map(_.name), Nil)
    assertEquals(op("increment").params.map(_.name), List("n"))

  test("method-level annotation is exposed (resolved rpcName + @rpcName readable)"):
    assertEquals(op("findRenamed").rpcName, "findOne")
    assertEquals(op("findRenamed").rpcNameAnnot.map(_.name), Some("findOne"))
    // a non-renamed method has no @rpcName instance and its source == resolved name
    assertEquals(op("ping").rpcNameAnnot, None)
    assertEquals(op("ping").rpcName, "ping")

  test("per-param annotation is exposed on the param's metadata"):
    val nParam = op("increment").params.find(_.name == "n").get
    assert(nParam.multiAnnot.isDefined)
    // a param WITHOUT @multi reifies None
    val idParam = op("find").params.find(_.name == "id").get
    assertEquals(idParam.multiAnnot, None)
