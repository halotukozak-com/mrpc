package mrpc.meta

import made.*

import mrpc.derive.SampleApi.SampleApi
import mrpc.derive.TupleConstructionSpike

/**
 * Wave-0 type-preserving tuple-construction spike (Phase 10, Plan 01, Task 3).
 *
 * Resolves the CENTRAL locked-design risk (research §"Heterogeneous-Tuple Construction"): keeping
 * `done.operations` as the precisely-typed heterogeneous tuple — never widening to
 * `List[DoneOperation]` — through both an INLINE `mapAs` transform and a MACRO-side
 * `ofRefinedTuple` synthesis (see [[mrpc.derive.TupleConstructionSpike]]), both clean under
 * `-Ycheck:macros`.
 *
 * Note on the inline path: made's own `MappedTupleEvidenceTest` documents that reading a
 * per-element macro (`getAnnotation`/`label`) INSIDE `mapAs` does NOT compile — the element is seen
 * at its abstract `<: DoneOperation` bound, so the `{ type Label = L }` structural extension can't
 * reduce. The proven idiom (made `deriveProduct`) is therefore: drive `mapAs` with a plain accessor
 * under an explicit `F`, and read the refined per-element metadata via the tuple-level
 * `getAnnotations` delegation surface plus refinement-preserving `*:` destructuring. That is what
 * this spike exercises — the type-preservation guarantee `mapAs` provides on its OUTPUT tuple.
 */
class TupleConstructionSpikeSuite extends munit.FunSuite:

  private val done = summon[Done.Of[SampleApi]]
  // SampleApi declares 9 ops (ping, increment, find, users, lookup, lookup, combine, echoBool,
  // findRenamed) — the arity every type-preserving transform below must keep.
  private val opCount = 9

  test("inline mapAs path keeps per-element refinement and arity (no List widening)"):
    // `mapAs[made.DoneOperation]` drives a plain accessor (`inputElems.size`) under an explicit
    // F = [_] =>> Int. The result is a precisely-typed `Tuple.Map[ops, [_] =>> Int]` whose arity
    // equals the op count — proving the heterogeneous tuple is preserved, not widened.
    val arities =
      done.operations
        .mapAs[made.DoneOperation][[o <: made.DoneOperation] =>> Int]([o <: made.DoneOperation] =>
          (op: o) => op.inputElems.size)
    assertEquals(arities.size, opCount)

    // The refined per-op metadata read uses the tuple-level delegation surface (per-element, no
    // `done.operations.toList` first) plus destructuring to keep refinement.
    val names: List[Option[String]] =
      done.operations.getAnnotations[mrpc.annotation.rpcName].toList
        .map(_.asInstanceOf[Option[mrpc.annotation.rpcName]].map(_.name))
    assertEquals(names.size, opCount)
    assert(names.exists(_.contains("findOne")))

    // First op's label resolves through destructuring (refinement preserved).
    val (pingOp *: _) = done.operations: @unchecked
    assertEquals(pingOp.label, "ping")

  test("macro-side ofRefinedTuple synthesis is -Ycheck:macros clean and yields the op labels"):
    // Builds a refined tuple of Expr[String] (one label per op), folds with the ofRefinedTuple
    // clone, and unpacks back to a List[String]. Compiling under -Ycheck:macros is the proof.
    val labels = TupleConstructionSpike.opLabels[SampleApi]
    assertEquals(labels.size, opCount)
    assertEquals(
      labels.toSet,
      Set("ping", "increment", "find", "users", "lookup", "combine", "echoBool", "findRenamed"),
    )
