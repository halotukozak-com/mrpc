package mrpc
package meta

import made.Made
import made.Made.Of
import mrpc.annotation.{composite, multi, reifyName, rpcMethodMetadata}
import mrpc.derive.SampleApi.SampleApi

// The @composite fixture (research §"@composite detail"): NameInfo groups two name reads of the SAME
// real symbol. @composite means "flatten NameInfo's params into the enclosing construction against the
// SAME real symbol context" — so NameInfo.name is the source label and NameInfo.rpcName is the resolved
// rpcName of the enclosing METHOD, NOT the NameInfo field names.
final case class NameInfo(
  @reifyName name: String,
  @reifyName(useRawName = true) rpcName: String,
)

// Per-method metadata whose name reads are grouped into a @composite NameInfo sub-value.
final case class MethodMetaCmp[T](
  @composite nameInfo: NameInfo,
) extends TypedMetadata[T]

// Trait-level metadata: one MethodMetaCmp per RPC method (a 2-level tree: trait -> method -> nameInfo).
final case class TraitMetaCmp[T](
  @rpcMethodMetadata @multi methods: List[MethodMetaCmp[?]],
)
object TraitMetaCmp extends RpcMetadataCompanion[TraitMetaCmp]

// META2-02 @composite assertions: recursion against the SAME real symbol context.
class CompositeMetadataSuite extends munit.FunSuite:

  private lazy val md: TraitMetaCmp[SampleApi] = TraitMetaCmp.materialize[SampleApi]
  private def method(source: String): MethodMetaCmp[?] =
    md.methods.find(_.nameInfo.name == source).get

  test("@composite NameInfo reads the REAL method symbol (not the NameInfo field names)"):
    // findRenamed: source label = "findRenamed", resolved rpcName = "findOne" (@rpcName).
    val renamed = method("findRenamed")
    assertEquals(renamed.nameInfo.name, "findRenamed")
    assertEquals(renamed.nameInfo.rpcName, "findOne")

  test("@composite threads the same context for an un-renamed method (name == rpcName)"):
    val ping = method("ping")
    assertEquals(ping.nameInfo.name, "ping")
    assertEquals(ping.nameInfo.rpcName, "ping")

  test("a 2-level tree: trait -> one MethodMetaCmp per op -> a filled nested NameInfo"):
    assertEquals(md.methods.size, 9)
    assertEquals(
      md.methods.map(_.nameInfo.name).toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed"),
    )
    // every method's composite is materialized (no null/empty nested value)
    assert(md.methods.forall(_.nameInfo.rpcName.nonEmpty))
