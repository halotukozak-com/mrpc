package halotukozak.mrpc
package annotation

import made.annotation.MetaAnnotation

import scala.quoted.{Expr, FromExpr, Quotes}

/** Name prefix for RPC methods. Mirrors commons `rpcNamePrefix(prefix, overloadedOnly = false)`. */
final class rpcNamePrefix(val prefix: String, val overloadedOnly: Boolean) extends MetaAnnotation:
  def this(prefix: String) = this(prefix, false)

private[mrpc] object rpcNamePrefix:
  given FromExpr[rpcNamePrefix]:
    def unapply(expr: Expr[rpcNamePrefix])(using quotes: Quotes): Option[rpcNamePrefix] = expr match
      case '{ new `rpcNamePrefix`(${ Expr(prefix) }, ${ Expr(overloaded) }) } =>
        Some(new rpcNamePrefix(prefix, overloaded))
      case '{ new `rpcNamePrefix`(${ Expr(prefix) }) } =>
        Some(new rpcNamePrefix(prefix, false))
      case _ => None
