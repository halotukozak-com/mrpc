package mrpc
package derive

import scala.annotation.switch
import scala.quoted.*

//it could be done without macro, with inline if else, but @switch for performance
inline def matchFrom[Names <: Tuple, Values <: Tuple](
  args: NamedTuple.NamedTuple[Names, Values],
)[R /* <: Tuple.Union[Values] */ ](
  inline scrutinee: String,
  inline reject: R,
): R = ${ matchFromImpl[Names, Values, R]('scrutinee, 'args, 'reject) }

def matchFromImpl[Names <: Tuple: Type, Values <: Tuple: Type, R: Type](
  scrutinee: Expr[String],
  args: Expr[NamedTuple.NamedTuple[Names, Values]],
  reject: Expr[R],
)(using Quotes,
): Expr[R] =
  import quotes.reflect.*

  val caseDefs = TupleTraverse.traverseTuple[Names, String].zipWithIndex.map { (name, index) =>
    CaseDef(Expr(Type.valueOfConstant(using name).get).asTerm, None, '{ $args(${ Expr(index) }).asInstanceOf[R] }.asTerm)
  }
  val default = CaseDef(Wildcard(), None, reject.asTerm)
  Match('{ $scrutinee: @switch }.asTerm, caseDefs :+ default).asExprOf[R]
