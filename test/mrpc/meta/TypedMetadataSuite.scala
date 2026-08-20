package halotukozak.mrpc.meta

import halotukozak.mrpc.annotation.{infer, multi, reifyAnnot, reifyName, rpcMethodMetadata}
import halotukozak.mrpc.derive.SampleApi.SampleApi

// A trivially-summonable given typeclass, so @infer is deterministic regardless of the result type.
final class MethodTag[T]
object MethodTag:
  val instance: MethodTag[Any] = new MethodTag[Any]
  given derived[T]: MethodTag[T] = instance.asInstanceOf[MethodTag[T]]

// Per-method metadata: source name, resolved name, the single @rpcName annotation, an inferred tag.
final case class MethodMeta[T](
  @reifyName name: String,
  @reifyName(useRawName = true) rawName: String,
  @reifyAnnot rpcNameAnnot: Option[halotukozak.mrpc.annotation.rpcName],
  @infer tag: MethodTag[T],
) extends TypedMetadata[T]

// Trait-level metadata collecting one MethodMeta per RPC method.
final case class ApiMeta[T](
  @rpcMethodMetadata @multi methods: List[MethodMeta[?]],
)
object ApiMeta extends RpcMetadataCompanion[ApiMeta]

// RED until Task 2 (the materialize macro) lands. The fixture metadata classes above ENCODE the
// META2-01 target behavior: @reifyName (source + resolved), @reifyAnnot (single), @infer.
class TypedMetadataSuite extends munit.FunSuite:

  test("@reifyName source == made label; @reifyName(useRawName) == resolved rpcName"):
    val md = ApiMeta.materialize[SampleApi]
    val findRenamed = md.methods.find(_.name == "findRenamed").get
    // source name is the made label (pre-resolution)
    assertEquals(findRenamed.name, "findRenamed")
    // resolved name honors @rpcName("findOne") via RpcName.computeAll
    assertEquals(findRenamed.rawName, "findOne")
    // a non-renamed method's source and resolved names coincide
    val ping = md.methods.find(_.name == "ping").get
    assertEquals(ping.name, "ping")
    assertEquals(ping.rawName, "ping")

  test("@reifyAnnot (single) holds the real @rpcName instance"):
    val md = ApiMeta.materialize[SampleApi]
    val findRenamed = md.methods.find(_.name == "findRenamed").get
    assertEquals(findRenamed.rpcNameAnnot.map(_.name), Some("findOne"))
    // a method WITHOUT @rpcName reifies None
    val ping = md.methods.find(_.name == "ping").get
    assertEquals(ping.rpcNameAnnot, None)

  test("@infer slot is the summoned given for the metadata type param"):
    val md = ApiMeta.materialize[SampleApi]
    // every method's @infer slot resolves to the same MethodTag given instance
    assert(md.methods.forall(_.tag eq MethodTag.instance))
