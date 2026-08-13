package halotukozak.mrpc
package meta

import halotukozak.made.*
import halotukozak.mrpc.derive.SampleApi.SampleApi
import halotukozak.commons.*

/**
 * Wave-0 type-preserving tuple-construction spike (Phase 10, Plan 01, Task 3).
 *
 * Resolves the CENTRAL locked-design risk (research §"Heterogeneous-Tuple Construction"): keeping
 * `done.operations` as the precisely-typed heterogeneous tuple — never widening to
 * `List[DoneOperation]` — through an INLINE `mapAs` transform, clean under `-Ycheck:macros`.
 *
 * (A macro-side counterpart once explored building a precisely-typed refined tuple of per-op Exprs
 * for this same risk; the macro that actually shipped, `AsRealDerivation`, took the simpler route of
 * a widened `Tuple` instead, so that half of the spike was removed as unused.)
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
    // `mapAs[halotukozak.made.DoneOperation]` drives a plain accessor (`inputElems.size`) under an explicit
    // F = [_] =>> Int. The result is a precisely-typed `Tuple.Map[ops, [_] =>> Int]` whose arity
    // equals the op count — proving the heterogeneous tuple is preserved, not widened.
    val arities =
      done.operations
        .mapAs[halotukozak.made.DoneOperation][[o <: halotukozak.made.DoneOperation] =>> Int](
          [o <: halotukozak.made.DoneOperation] => (op: o) => op.inputElems.size,
        )
    assertEquals(arities.size, opCount)

    // The refined per-op metadata read uses the tuple-level delegation surface (per-element, no
    // `done.operations.toList` first) plus destructuring to keep refinement.
    val names: List[Option[String]] =
      done.operations
        .getAnnotations[halotukozak.mrpc.annotation.rpcName]
        .toList
        .map(x => Option(x.asInstanceOf[halotukozak.mrpc.annotation.rpcName | Null]).map(_.name))
    assertEquals(names.size, opCount)
    assert(names.exists(_.contains("findOne")))

    // First op's label resolves through destructuring (refinement preserved).
    val (pingOp *: _) = done.operations: @unchecked
    assertEquals(pingOp.label, "ping")
