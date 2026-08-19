package halotukozak.mrpc.meta

import halotukozak.mrpc.annotation.{multi, reifyName, rpcMethodMetadata, rpcParamMetadata}
import halotukozak.mrpc.derive.SampleApi.SampleApi

// Per-param metadata: just the source name.
final case class ParamMeta[T](
  @reifyName name: String,
) extends TypedMetadata[T]

// Per-method metadata: source name + the per-param projection (declaration order).
final case class MethodMetaMC[T](
  @reifyName name: String,
  @rpcParamMetadata @multi params: List[ParamMeta[?]],
) extends TypedMetadata[T]

// Trait-level metadata: one MethodMetaMC per RPC method.
final case class TraitMeta[T](
  @rpcMethodMetadata @multi methods: List[MethodMetaMC[?]],
)
object TraitMeta extends RpcMetadataCompanion[TraitMeta]

// RED until Task 2 (the materialize macro) lands. Encodes META2-02 multi-collection behavior:
// @rpcMethodMetadata/@rpcParamMetadata with @multi, and the no-fork invariant.
class MultiCollectionSuite extends munit.FunSuite:

  test("@rpcMethodMetadata @multi: one entry per RPC method (10 ops)"):
    val md = TraitMeta.materialize[SampleApi]
    assertEquals(md.methods.size, 10)

  test("method names match the resolved rpcNames (source label here)"):
    val md = TraitMeta.materialize[SampleApi]
    assertEquals(
      md.methods.map(_.name).toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed", "configure"),
    )

  test("@rpcParamMetadata @multi: params in declaration order for combine"):
    val md = TraitMeta.materialize[SampleApi]
    val combine = md.methods.find(_.name == "combine").get
    assertEquals(combine.params.map(_.name), List("a", "b", "c"))
    val ping = md.methods.find(_.name == "ping").get
    assertEquals(ping.params.map(_.name), Nil)

  test("op labels cover every method of the trait (no-fork by construction, shared Done walk)"):
    val md = TraitMeta.materialize[SampleApi]
    // Both this metadata path and the engine walk the SAME `Done.Of[SampleApi]` mirror, so the op set
    // can't fork — asserted directly against the known fixture label set (resolved-name equality is
    // asserted in ApiMeta's @reifyName(useRawName) path, MetadataSuite).
    assertEquals(
      md.methods.map(_.name).toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed", "configure"),
    )
