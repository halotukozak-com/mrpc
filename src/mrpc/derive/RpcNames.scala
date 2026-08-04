package mrpc.derive

import made.*

import scala.quoted.*

/**
 * Whole-trait resolved RPC names lifted to the type level: `RpcNames[T].Underlying` is a tuple of
 * singleton string types, one per operation, in `Done.Operations` order.
 *
 * Deliberately NOT a per-op typeclass folded via `compiletime.summonAll[Tuple.Map[Done.Operations,
 * RpcName]]`: `Tuple.Map`'s per-element type-variable inference drops any `MetaAnnotation` the op
 * carries (e.g. `@multi`) before a per-op derivation could read it. Instead `deriveImpl` walks
 * `Done.Operations` reflectively in one pass — annotations stay intact in `TypeRepr` — resolves
 * `@rpcName`/`@rpcNamePrefix` and overload suffixes value-side, then emits the resolved names as types.
 */
sealed trait RpcNames[T]:
  type Underlying <: Tuple
  given Underlying containsOnly String = containsOnly.refl

object RpcNames:

  transparent inline given derived[T: Done.Of as done]: RpcNames[T] = ${
    deriveImpl[T, Tuple.Map[
      done.Operations,
      [Op] =>> Op match
        case ([l0 <: String] =>> DoneOperation { type Label = l0 })[l] => l,
    ], done.Operations]
  }

  private def deriveImpl[T: Type, Labels <: Tuple: Type, Operations <: Tuple: Type](using Quotes): Expr[RpcNames[T]] =
    val ops = TupleTraverse.traverseTuple[Operations, DoneOperation]
    val bases = ops.map:
      case '[type l <: String; type meta <: Tuple; { type Label = l; type Metadata = meta }] =>
        OpReflect
          .findAnnotation[mrpc.annotation.rpcName, meta]
          .map(_.name)
          .getOrElse(Type.valueOfConstant[l].get)

    // Group by base name to detect overloads: a base shared by >1 op is an overloaded set.
    val baseCounts: Map[String, Int] = bases.groupBy(identity).view.mapValues(_.size).toMap

    val resolved = ops
      .zip(bases)
      .map:
        case ('[type elems <: Tuple; type meta <: Tuple; { type InputElems = elems; type Metadata = meta }], base) =>
          val overloaded = baseCounts.getOrElse(base, 0) > 1
          val prefixed = applyPrefix[meta](base, overloaded)
          // Only overloaded members without their own @rpcName disambiguation get the signature suffix.
          if overloaded && !OpReflect.hasAnnotation[mrpc.annotation.rpcName, meta] then prefixed + overloadSuffix[elems]
          else prefixed
        case (_, _) => ???

    val labels = Type.valueOfTuple[Labels].get.toList.asInstanceOf[List[String]]

    detectDuplicates(labels, resolved)

    Expr.ofRefinedTuple(resolved.map(Expr(_))) match
      case '{ type ns <: Tuple; $_ : ns } =>
        '{
          (new RpcNames[T]:
            type Underlying = ns
          ): RpcNames[T] { type Underlying = ns }
        }

  /** Applies `@rpcNamePrefix` per its `overloadedOnly` flag. */
  private def applyPrefix[Metadata <: Tuple: Type](base: String, overloaded: Boolean)(using Quotes): String =
    OpReflect.findAnnotation[mrpc.annotation.rpcNamePrefix, Metadata] match
      case None => base
      case Some(annot) =>
        if !annot.overloadedOnly || overloaded then annot.prefix + base else base

  /**
   * Deterministic, reorder-stable suffix for an overloaded method: `_` followed by a positive
   * hexadecimal hash of the flattened parameter type `show` strings joined by a separator. Distinct
   * signatures yield distinct suffixes; reordering the overloads does not change any one suffix.
   */
  private def overloadSuffix[Elems <: Tuple: Type](using Quotes): String =
    val sig = TupleTraverse
      .traverseTuple[Elems, InputElem]
      .map:
        case '[{ type Type = t }] => Type.show[t]
      .mkString("(", ",", ")")
    val hash = sig.hashCode & 0x7fffffff
    s"_${hash.toHexString}"

  /** Aborts compilation if two non-overload ops compute the same final name. */
  private def detectDuplicates(labels: List[String], resolved: List[String])(using Quotes): Unit =
    import quotes.reflect.*
    resolved
      .zip(labels)
      .groupMap((name, _) => name)((_, label) => label)
      .find((_, ls) => ls.sizeIs > 1)
      .foreach: (name, ls) =>
        report.errorAndAbort(
          s"duplicate RPC name '$name' computed for methods ${ls.mkString(", ")}; disambiguate with @rpcName",
        )
