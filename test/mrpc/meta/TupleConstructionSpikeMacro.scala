package mrpc.derive

import scala.quoted.*

import made.*

/**
 * Macro-side companion to [[mrpc.meta.TupleConstructionSpikeSuite]] (Phase 10, Plan 01, Task 3).
 *
 * Lives in `mrpc.derive` so it can reach the `private[mrpc]` `Matcher.operationTypes` and
 * `OpReflect.labelOf` engine introspection, and in a SEPARATE file from the suite because a macro
 * cannot be invoked in the same source file it is defined in. Proves the macro-side half of the
 * central locked-design risk: building a precisely-typed refined tuple of per-op metadata Exprs via
 * [[RefinedTupleExpr.ofRefinedTuple]] (the clone of made's helper) and unpacking it back — all
 * `-Ycheck:macros` clean.
 */
object TupleConstructionSpike:

  /** Builds a refined tuple of each op's label, then unpacks it back to a `List[String]`. */
  inline def opLabels[T]: List[String] = ${ opLabelsImpl[T] }

  private def opLabelsImpl[T: Type](using Quotes): Expr[List[String]] =
    import quotes.reflect.*
    // Summon the mirror exactly as the Plan 02 macro will, then read op types via the engine path.
    val doneExpr = Expr.summon[Done.Of[T]].getOrElse(report.errorAndAbort(s"no Done.Of[${Type.show[T]}]"))
    val opTypes = Matcher.operationTypes[T](doneExpr)

    // One trivial Expr[String] (the label) per op — the representative per-op metadata Expr list.
    val labelExprs: List[Expr[String]] = opTypes.map(opTpe => Expr(OpReflect.labelOf(opTpe)))

    // Fold into a precisely-typed refined tuple, then unpack via the `'{ type x <: Tuple; $e: x }`
    // idiom (mirrors Done.derived / made getAnnotationsImpl). The refined tuple's element types
    // survive the synthesis — the proof is that this compiles AND `-Ycheck:macros` is clean.
    RefinedTupleExpr.ofRefinedTuple(labelExprs) match
      case '{ type x <: Tuple; $tup: x } =>
        '{ $tup.toList.map(_.asInstanceOf[String]) }
