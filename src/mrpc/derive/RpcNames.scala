package mrpc.derive

import made.*

import scala.quoted.*

object RpcNames:
  private type Proxy0 = TypeProxy { type Underlying <: Tuple }
  opaque type Proxy[T] <: Proxy0 = Proxy0

  given [T, Under <: Tuple, P <: Proxy[T] { type Underlying = Under }] => (Under containsOnly String) =
    containsOnly.refl

  transparent inline def materialize[T: Done.Of as done]: Proxy[T] =
    ${
      materializeImpl[T, Tuple.Map[
        done.Operations,
        [Op] =>> Op match
          case ([l0 <: String] =>> DoneOperation { type Label = l0 })[l] => l,
      ], done.Operations]
    }

  private def materializeImpl[T: Type, Labels <: Tuple: Type, Operations <: Tuple: Type](using Quotes): Expr[Proxy[T]] =
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
        '{ TypeProxy[ns] }

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
