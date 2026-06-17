package mrpc.derive

import scala.quoted.*

/**
 * Type-level tuple traversal used by the matcher to walk `Done.Operations`, an op's `InputElems`,
 * and `Metadata` chains. made ships the same `'[t *: ts]` recursion as `traverseTuple`, but it is
 * `private[made]` (MacroUtils.scala), so it cannot be imported here — the three-line recursion is
 * reimplemented verbatim.
 */
private[derive] object TupleTraverse:

  def traverseTuple(tpe: Type[? <: Tuple])(using Quotes): List[Type[? <: AnyKind]] =
    tpe match
      case '[EmptyTuple] => Nil
      case '[t *: ts] => Type.of[t] :: traverseTuple(Type.of[ts])
