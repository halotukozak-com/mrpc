package mrpc.derive

import made.*
import scala.quoted.*

/**
 * Whole-trait resolved RPC names lifted to the type level: `RpcNames[T].Names` is a tuple of singleton
 * string types — one per operation, in `Done.Operations` order — equal to [[RpcNames.deriveImpl]]'s
 * result. `names` is the same tuple as a runtime value.
 *
 * Deliberately NOT a per-op typeclass derived independently for each operation and folded via
 * `compiletime.summonAll[Tuple.Map[Done.Operations, RpcName]]`: that shape pushes each op through
 * `Tuple.Map`'s per-element type-variable inference, which drops any `MetaAnnotation` the op carries
 * (e.g. `@multi`) — annotations don't survive being abstracted into a generic type variable. Instead
 * `RpcNames` walks `Done.Operations` reflectively in ONE PASS (annotations stay intact in `TypeRepr`),
 * reads `@rpcName`/`@rpcNamePrefix` terms and computes overload suffixes value-side (see `baseName`/
 * `applyPrefix`/`overloadSuffix` below), then EMITS the resolved names as types — which is exactly what
 * `Tuple.Map`/match types cannot do. **Annotation-proof** by construction.
 */
sealed trait RpcNames[T]:
  type Names <: Tuple /* of String */
  
  given Names containsOnly String = containsOnly.refl

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
    val bases = ops.map: op =>
      OpReflect
        .findAnnotation[mrpc.annotation.rpcName](op)
        .map(_.name)
        .getOrElse(op match
          case '[type l <: String; { type Label = l }] => Type.valueOfConstant[l].get)

    // Group by base name to detect overloads: a base shared by >1 op is an overloaded set.
    val baseCounts: Map[String, Int] = bases.groupBy(identity).view.mapValues(_.size).toMap

    val resolved = ops
      .zip(bases)
      .map: (op, base) =>
        val overloaded = baseCounts.getOrElse(base, 0) > 1
        val prefixed = applyPrefix(op, base, overloaded)
        // Only overloaded members without their own @rpcName disambiguation get the signature suffix.
        if overloaded && !OpReflect.hasAnnotation[mrpc.annotation.rpcName](op) then prefixed + overloadSuffix(op)
        else prefixed

    val labels = Type.valueOfTuple[Labels].get.toList.asInstanceOf[List[String]]

    detectDuplicates(labels, resolved)

    Expr.ofRefinedTuple(resolved.map(Expr(_))) match
      case '{ type ns <: Tuple; $_ : ns } =>
        '{
          (new RpcNames[T]:
            type Names = ns
          ): RpcNames[T] { type Names = ns }
        }

  /** Applies `@rpcNamePrefix` per its `overloadedOnly` flag. */
  private def applyPrefix(op: Type[? <: DoneOperation], base: String, overloaded: Boolean)(using Quotes): String =
    OpReflect.findAnnotation[mrpc.annotation.rpcNamePrefix](op) match
      case None => base
      case Some(annot) =>
        if !annot.overloadedOnly || overloaded then annot.prefix + base else base

  /**
   * Deterministic, reorder-stable suffix for an overloaded method: `_` followed by a positive
   * hexadecimal hash of the flattened parameter type `show` strings joined by a separator. Distinct
   * signatures yield distinct suffixes; reordering the overloads does not change any one suffix.
   */
  private def overloadSuffix(op: Type[?])(using Quotes): String =
    val sig = op match
      case '[type elems <: Tuple; DoneOperation { type InputElems = elems }] =>
        TupleTraverse
          .traverseTuple[elems, InputElem]
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
