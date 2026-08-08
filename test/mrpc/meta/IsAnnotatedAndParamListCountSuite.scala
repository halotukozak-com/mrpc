package mrpc.meta

import mrpc.annotation.{isAnnotated, multi, reifyName, reifyParamListCount, rpcMethodMetadata, rpcName}
import mrpc.derive.SampleApi.SampleApi

/**
 * DIVERGENCES.md D16: `@isAnnotated[A]` (plain presence check, unlike `@reifyAnnot`'s arity-shaped
 * extraction) and `@reifyParamListCount` (method-level parameter-list count).
 */
final case class MethodFlags[T](
  @reifyName name: String,
  @isAnnotated[rpcName] hasRpcNameOverride: Boolean,
  @reifyParamListCount paramListCount: Int,
)

final case class TraitFlags[T](
  @rpcMethodMetadata @multi methods: List[MethodFlags[?]],
)
object TraitFlags extends RpcMetadataCompanion[TraitFlags]

class IsAnnotatedAndParamListCountSuite extends munit.FunSuite:

  test("@isAnnotated[rpcName] is true only for the method carrying @rpcName"):
    val md = TraitFlags.materialize[SampleApi]
    val flagged = md.methods.filter(_.hasRpcNameOverride).map(_.name)
    assertEquals(flagged, List("findRenamed"))

  test("@isAnnotated[rpcName] is false for every other method"):
    val md = TraitFlags.materialize[SampleApi]
    val notFlagged = md.methods.filterNot(_.hasRpcNameOverride).map(_.name).toSet
    assertEquals(notFlagged, Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool"))

  test("@reifyParamListCount is 2 for combine(a)(b, c) and 1 for a single-list method"):
    val md = TraitFlags.materialize[SampleApi]
    assertEquals(md.methods.find(_.name == "combine").get.paramListCount, 2)
    assertEquals(md.methods.find(_.name == "find").get.paramListCount, 1)
    assertEquals(md.methods.find(_.name == "increment").get.paramListCount, 1)
