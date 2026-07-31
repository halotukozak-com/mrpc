package mrpc.annotation

import scala.quoted.{Expr, FromExpr, Quotes}

/**
 * Steers a metadata-class constructor parameter to be materialized by IMPLICIT SEARCH
 * (`Expr.summon`) for the parameter's declared type. The optional `clue` is surfaced in the
 * error message when the implicit cannot be found.
 *
 * Read by reflection on the metadata class's constructor params — NOT captured in
 * `made.Done.Metadata`, so this is a plain [[scala.annotation.StaticAnnotation]]. Mirrors commons
 * `infer`.
 */
final class infer(val clue: String) extends scala.annotation.StaticAnnotation:
  def this() = this("")

private[mrpc] object infer:
  given FromExpr[infer] with
    override def unapply(x: Expr[infer])(using Quotes): Option[infer] = x match
      case '{ new `infer`(${ Expr(clue) }) } => Some(new infer(clue))
      case '{ new `infer`() } => Some(new infer(""))
      case _ => None
