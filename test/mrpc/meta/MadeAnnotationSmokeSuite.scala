package mrpc.meta

import made.*
import mrpc.annotation.{multi, rpcName}
import mrpc.derive.SampleApi.SampleApi

/**
 * Wave-0 made-artifact smoke (Phase 10, Plan 01, Task 1).
 *
 * Resolves research Open Question 1 / Pitfall 2: does the published
 * `io.github.halotukozak::made:0.1.3-done-SNAPSHOT` artifact actually capture PARAM-level
 * annotations (not just method-level)? If method annotations reify but param annotations come
 * back `EmptyTuple`, the param-metadata projection in Plan 02/03 is dead on arrival.
 *
 * The per-element reads keep each op/param's precise `{ type Metadata = M; type Label = L }`
 * refinement: method annotations are read via the tuple-level `getAnnotations` delegation surface,
 * and the param read destructures the operation tuple with `*:` so the matched `DoneOperation`
 * keeps its refinement and made's `getAnnotation`/`label`/`inputElems` extensions resolve. We never
 * widen `done.operations` to `List[DoneOperation]` before delegating to made.
 *
 * If the param read returns `None`, the suite fails LOUDLY (see `failParamCapture`) with a
 * publishLocal instruction rather than passing silently.
 */
class MadeAnnotationSmokeSuite extends munit.FunSuite:

  // The Done mirror Plan 02's macro will delegate to. Summoned for the primary RPC trait.
  private val done = summon[Done.Of[SampleApi]]

  private def failParamCapture(): Nothing =
    fail(
      "made SNAPSHOT does not capture PARAM-level annotations (@multi on increment's `n` reified " +
        "nothing). The published artifact predates param-capture. Rebuild it: " +
        "`cd ~/IdeaProjects/made && scala-cli --power publish local .` from a commit >= 16bfbdb, " +
        "then re-run this suite.",
    )

  test("method-level annotation read via getAnnotations is a tuple of Option[rpcName]"):
    // The per-element delegation surface Plan 02 relies on. `getAnnotations` reads each op's
    // refined Metadata WITHOUT widening to a List first; we materialize to a List only afterwards.
    val annots = done.operations.getAnnotations[rpcName].toList.map(_.asInstanceOf[rpcName | Null])
    assertEquals(annots.size, done.operations.toList.size)
    // findRenamed carries @rpcName("findOne") — the resolved instance is readable.
    assert(annots.exists(x => x != null && x.name == "findOne"), s"expected findOne among $annots")

  test("param-level annotation read: increment's `n` carries @multi (research Pitfall 2)"):
    // SampleApi declares ops in source order: ping, increment, find, users, lookup, lookup,
    // combine, echoBool, findRenamed. Destructure to bind `increment` (2nd op) at its precise
    // refined type so made's `inputElems` + per-InputElem `getAnnotation` resolve.
    val (_ *: incrementOp *: _) = done.operations: @unchecked

    // The first (and only) param of increment carries @multi. Destructure inputElems to keep the
    // InputElem refinement, then delegate to made's getAnnotation on the param element.
    val (nParam *: _) = incrementOp.inputElems: @unchecked
    assertEquals(incrementOp.label, "increment")

    if nParam.getAnnotation[multi] == null then failParamCapture()
    assert(nParam.hasAnnotation[multi], "param-level @multi capture proven")

  test("label read: ping op's label equals its source name"):
    // ping is the first op; destructure to read its precise singleton Label.
    val (pingOp *: _) = done.operations: @unchecked
    assertEquals(pingOp.label, "ping")
