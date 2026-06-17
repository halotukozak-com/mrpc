package mrpc.derive

import scala.quoted.*

/**
 * `private[mrpc]` clone of made's `Expr.ofRefinedTuple` helper.
 *
 * made exposes the same helper as `extension (companion: Expr.type) private[made] def
 * ofRefinedTuple(...)` in `made/MacroUtils.scala` (lines ~48-54) — but it is `private[made]` and
 * therefore cannot be imported here. The Plan 02 materialize macro needs the SAME proven,
 * `-Ycheck:macros`-clean refined-tuple synthesis idiom (research Open Question 4), so we clone the
 * exact recursive body verbatim.
 *
 * The recursion pattern-matches the already-built tail at its precise singleton tuple type
 * (`'{ type t <: Tuple; $tailExpr: t }`) so each prepended head keeps its element refinement, and
 * the resulting `Expr[Tuple]` is a precisely-typed heterogeneous tuple rather than a widened
 * `Tuple` — this is what keeps made's per-element inline extensions resolvable downstream.
 *
 * @see made/MacroUtils.scala `ofRefinedTuple`
 */
private[mrpc] object RefinedTupleExpr:

  def ofRefinedTuple(exprs: List[Expr[?]])(using Quotes): Expr[Tuple] = exprs.runtimeChecked match
    case Nil => '{ EmptyTuple }
    case '{ $headExpr: h } :: tail =>
      ofRefinedTuple(tail) match
        case '{ type t <: Tuple; $tailExpr: t } =>
          '{ ${ headExpr.asExprOf[h] } *: ${ tailExpr.asExprOf[t] } }
