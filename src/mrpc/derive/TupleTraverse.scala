package mrpc
package derive

import scala.quoted.*

/**
 * Type-level tuple traversal used by the matcher to walk `Done.Operations`, an op's `InputElems`,
 * and `Metadata` chains. made ships the same `'[t *: ts]` recursion as `traverseTuple`, but it is
 * `private[made]` (MacroUtils.scala), so it cannot be imported here — the three-line recursion is
 * reimplemented verbatim.
 */
private[derive] object TupleTraverse:
  def traverseTuple[Tup <: Tuple: Type, T: Type](using Quotes): List[Type[? <: T]] = Type.of[Tup] match
    case '[EmptyTuple] => Nil
    case '[type t <: T; *:[t, ts]] => Type.of[t] :: traverseTuple[ts, T]
    case '[other] => throw MatchError(typeInfo[other])

  /** Folds element types into a `*:`-cons tuple type, in order — the inverse of [[traverseTuple]]. */
  def foldTuple(elems: List[Type[?]])(using Quotes): Type[? <: Tuple] =
    import quotes.reflect.*
    val folded: TypeRepr = elems.foldRight(TypeRepr.of[EmptyTuple]) { (elem, acc) =>
      (elem, acc.asType) match
        case ('[h], '[type t <: Tuple; t]) => TypeRepr.of[h *: t]
        case _ => acc
    }
    folded.asType match
      case '[type r <: Tuple; r] => Type.of[r]
