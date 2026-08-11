package mrpc
package derive

import scala.quoted.*
import commons.typeInfo

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

