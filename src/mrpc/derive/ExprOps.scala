package mrpc.derive

import scala.quoted.*

/**
 * `private[mrpc]` clone of made's `Expr.ofRefinedTuple` (`made/MacroUtils.scala`, `private[made]` so
 * it cannot be imported here) — folds a `List[Expr[?]]` into a `Tuple` term that keeps each element's
 * OWN precise type (`h1 *: h2 *: ... *: EmptyTuple`), unlike `Expr.ofTupleFromSeq` (`Seq[Expr[Any]]`),
 * which widens every element to `Any` before tupling.
 *
 * The distinction matters wherever the built tuple is spliced somewhere its precise static type is
 * read back — e.g. as the receiver of made's `handlers.to[Target]` (whose `Handlers` type parameter
 * is inferred from the receiver's type) or as an argument tuple checked against a path-dependent
 * `Args` type. A tuple built via `ofTupleFromSeq` would there be seen as the widened `Tuple`, not the
 * precise per-element shape.
 */
extension (companion: Expr.type)
  private[mrpc] def ofRefinedTuple(exprs: List[Expr[?]])(using Quotes): Expr[Tuple] = exprs.runtimeChecked match
    case Nil => '{ EmptyTuple }
    case '{ $headExpr: h } :: tail =>
      Expr.ofRefinedTuple(tail) match
        case '{ type t <: Tuple; $tailExpr: t } =>
          '{ ${ headExpr.asExprOf[h] } *: ${ tailExpr.asExprOf[t] } }
