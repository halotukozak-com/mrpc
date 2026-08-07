package mrpc

import scala.annotation.nowarn
import scala.compiletime.Erased
import scala.quoted.{Expr, Quotes, Type}

sealed class TypeProxy extends Erased:
  type Underlying

object TypeProxy:

  @nowarn("msg=duplicated at each inline site")
  inline def apply[T]: TypeProxy { type Underlying = T } = new TypeProxy { type Underlying = T }

  transparent inline def apply[T](inline t: T): TypeProxy { type Underlying <: T } =
    ${ applyImpl('t) }

  private def applyImpl[T: Type](t: Expr[T])(using quotes: Quotes): Expr[TypeProxy { type Underlying <: T }] =
    import quotes.reflect.*
    t.asTerm.tpe.widen.asType match
      case '[type t <: T; t] => '{ TypeProxy[t] }
