package mrpc.derive

import scala.quoted.*

/**
 * `private[mrpc]` clone of made's `Expr.ofRefinedTuple` (`private[made]`, so it cannot be imported
 * here) — folds a `List[Expr[?]]` into a `Tuple` term that keeps each element's own precise type
 * (`h1 *: h2 *: ... *: EmptyTuple`), unlike `Expr.ofTupleFromSeq`, which widens every element to `Any`.
 * Needed wherever the built tuple's precise static type is read back later, e.g. as the receiver of
 * made's `handlers.to[Target]`.
 */
extension (companion: Expr.type)
  private[mrpc] def ofRefinedTuple(exprs: List[Expr[?]])(using Quotes): Expr[Tuple] = exprs.runtimeChecked match
    case Nil => '{ EmptyTuple }
    case '{ $headExpr: h } :: tail =>
      Expr.ofRefinedTuple(tail) match
        case '{ type t <: Tuple; $tailExpr: t } =>
          '{ ${ headExpr.asExprOf[h] } *: ${ tailExpr.asExprOf[t] } }
