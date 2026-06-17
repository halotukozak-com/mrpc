package mrpc.derive

import scala.quoted.*

/**
 * RPC-name resolution with compile-time duplicate detection. Resolution order mirrors commons:
 *
 *   1. `@rpcName("...")` wins over the method label — it is the RPC-serialization identity.
 *   2. `@rpcNamePrefix(prefix, overloadedOnly)` prepends `prefix`: always when `!overloadedOnly`, or
 *      only on overloaded methods when `overloadedOnly`.
 *   3. Overloaded methods (multiple ops sharing the same base name) receive a DETERMINISTIC,
 *      REORDER-STABLE, signature-based suffix derived from their flattened parameter types — never a
 *      positional `_1`/`_2` (commons' own warning: positional suffixes break on API reordering).
 *      Non-overloaded names are left untouched.
 *
 * After resolution, any final name shared by operations that are NOT a legitimately-disambiguated
 * overload group is a collision and aborts compilation with a message naming both methods.
 */
private[derive] object RpcName:

  /** Resolves the final rpcName for every op member (positionally), aborting on a duplicate. */
  def computeAll[T: Type](using Quotes)(members: List[quotes.reflect.Symbol]): List[String] =
    val labels = members.map(OpReflect.labelOf)
    val annots = members.map(OpReflect.methodAnnotations) // member-level MetaAnnotation terms, once
    val bases = members.lazyZip(annots).map((m, a) => baseName(m, a))

    // Group by base name to detect overloads: a base shared by >1 op is an overloaded set.
    val baseCounts: Map[String, Int] = bases.groupBy(identity).view.mapValues(_.size).toMap

    val resolved = members.lazyZip(annots).lazyZip(bases).map { (member, anns, base) =>
      val overloaded = baseCounts.getOrElse(base, 0) > 1
      val prefixed = applyPrefix(anns, base, overloaded)
      // Only overloaded members without their own @rpcName disambiguation get the signature suffix.
      if overloaded && !OpReflect.hasAnnotation[mrpc.annotation.rpcName](anns) then prefixed + overloadSuffix[T](member)
      else prefixed
    }

    detectDuplicates(labels, resolved)
    resolved

  /** `@rpcName` value if present, otherwise the member's label. */
  private def baseName(using Quotes)(member: quotes.reflect.Symbol, anns: List[quotes.reflect.Term]): String =
    OpReflect.stringAnnotationArg[mrpc.annotation.rpcName](anns, "name").getOrElse(OpReflect.labelOf(member))

  /** Applies `@rpcNamePrefix` per its `overloadedOnly` flag. */
  private def applyPrefix(using Quotes)(anns: List[quotes.reflect.Term], base: String, overloaded: Boolean): String =
    OpReflect.stringAnnotationArg[mrpc.annotation.rpcNamePrefix](anns, "prefix") match
      case None => base
      case Some(prefix) =>
        val overloadedOnly =
          OpReflect.booleanAnnotationArg[mrpc.annotation.rpcNamePrefix](anns, "overloadedOnly").getOrElse(false)
        if !overloadedOnly || overloaded then prefix + base else base

  /**
   * Deterministic, reorder-stable suffix for an overloaded method: `_` followed by a positive
   * hexadecimal hash of the flattened parameter type `show` strings joined by a separator. Distinct
   * signatures yield distinct suffixes; reordering the overloads does not change any one suffix.
   */
  private def overloadSuffix[T: Type](using Quotes)(member: quotes.reflect.Symbol): String =
    import quotes.reflect.*
    val sig = OpReflect
      .params[T](member)
      .map(p => TypeRepr.of(using p.tpe).show)
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
