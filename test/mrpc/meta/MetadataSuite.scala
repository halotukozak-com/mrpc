package mrpc.meta

import mrpc.annotation.rpcName
import mrpc.derive.SampleApi.*

// RED until the metadata materializer lands; see phase plan 02/03.
class MetadataSuite extends munit.FunSuite:
  object MetaFixture extends RpcMetadataCompanion

  private lazy val md: RpcMetadata[SampleApi] = MetaFixture.materializeMetadata[SampleApi]
  private def op(label: String): OperationMetadata = md.operations.find(_.label == label).get

  test("operations lists every op of the trait"):
    assertEquals(
      md.operations.map(_.label).toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed"),
    )

  test("arity tag is fire/call/get per output type"):
    assertEquals(op("ping").arity, "fire")
    assertEquals(op("increment").arity, "call")
    assertEquals(op("users").arity, "get")

  test("params are listed in declaration order, flattened across param lists"):
    assertEquals(op("combine").params.map(_.name), List("a", "b", "c"))
    assertEquals(op("ping").params.map(_.name), Nil)
    assertEquals(op("increment").params.map(_.name), List("n"))

  test("method-level annotation is exposed (resolved rpcName + @rpcName readable)"):
    assertEquals(op("findRenamed").name, "findOne")
    assert(op("findRenamed").hasAnnotation[rpcName])

  test("per-param annotation is exposed on the param's metadata"):
    val nParam = op("increment").params.find(_.name == "n").get
    assert(nParam.hasAnnotation[mrpc.annotation.multi])

  test("resolved rpcNames EQUAL the engine's (shared Done path, no fork)"):
    // The engine's resolved names come from Matcher.describe[SampleApi] (package mrpc.derive). Plan 02
    // widens Matcher to private[mrpc] so this cross-package assertion compiles.
    val engineByLabel: Map[String, List[String]] =
      mrpc.derive.Matcher.describe[SampleApi].groupMap(_.label)(_.rpcName)
    val metaByLabel: Map[String, List[String]] =
      md.operations.groupMap(_.label)(_.name)
    assertEquals(metaByLabel, engineByLabel)
