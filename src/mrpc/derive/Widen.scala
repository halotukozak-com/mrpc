package halotukozak.mrpc
package derive

import scala.quoted.*

transparent inline def widen[T](inline a: T): T = ${ widenImpl[T]('a) }
private def widenImpl[T: Type](a: Expr[T])(using quotes: Quotes): Expr[T] =
  import quotes.reflect.*
  a.asTerm.tpe.asType match
    case '[type t <: T; t] =>
      '{ compiletime.constValue[t] }
