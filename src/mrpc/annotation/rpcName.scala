package halotukozak.mrpc.annotation

import halotukozak.made.annotation.MetaAnnotation

import scala.quoted.*

/** Real-side name override for an RPC method. Mirrors commons `rpcName(name)`. */
final class rpcName(val name: String) extends MetaAnnotation

private[mrpc] object rpcName:
  given FromExpr[rpcName]:
    def unapply(expr: Expr[rpcName])(using Quotes): Option[rpcName] = expr match
      case '{ new `rpcName`(${ Expr(name) }) } => Some(new rpcName(name))
      case _ => None
