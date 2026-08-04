package mrpc
package derive

import scala.annotation.switch
import scala.quoted.*

inline def matchFrom[Names <: Tuple, Values <: Tuple](
  inline scrutinee: String,
  args: NamedTuple.NamedTuple[Names, Values],
  inline reject: Tuple.Union[Values],
) = ${ matchFromImpl[Names, Values]('scrutinee, 'args, 'reject) }

def matchFromImpl[Names <: Tuple: Type, Values <: Tuple: Type](
  scrutinee: Expr[String],
  args: Expr[NamedTuple.NamedTuple[Names, Values]],
  reject: Expr[Tuple.Union[Values]],
)(using Quotes,
): Expr[Tuple.Union[Values]] =
  import quotes.reflect.*

  val caseDefs = TupleTraverse.traverseTuple[Names, String].zipWithIndex.map { (name, index) =>
    CaseDef(Expr(Type.valueOfConstant(using name).get).asTerm, None, '{ $args(${ Expr(index) }) }.asTerm)
  }
  val default = CaseDef(Wildcard(), None, reject.asTerm)
  Match('{ $scrutinee: @switch }.asTerm, caseDefs :+ default).asExprOf[Tuple.Union[Values]]
