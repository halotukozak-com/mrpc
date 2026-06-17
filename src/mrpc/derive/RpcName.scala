package mrpc.derive

import scala.quoted.*

/**
 * RPC-name resolution with compile-time duplicate detection. Resolution order mirrors commons:
 *
 *   1. `@rpcName("...")` wins over the method label (and over made's `@name`, which already feeds the
 *      label) — it is the RPC-serialization identity.
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

  /** Resolves the final rpcName for every op (positionally), aborting on a duplicate. */
  def computeAll(ops: List[Type[?]])(using Quotes): List[String] =
    val labels = ops.map(OpReflect.labelOf)
    val bases = ops.map(baseName)

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

    detectDuplicates(labels, resolved)
    resolved

  /** `@rpcName` value if present, otherwise the op's label. */
  private def baseName(op: Type[?])(using Quotes): String =
    OpReflect.stringAnnotationArg[mrpc.annotation.rpcName](op, "name").getOrElse(OpReflect.labelOf(op))

  /** Applies `@rpcNamePrefix` per its `overloadedOnly` flag. */
  private def applyPrefix(op: Type[?], base: String, overloaded: Boolean)(using Quotes): String =
    OpReflect.stringAnnotationArg[mrpc.annotation.rpcNamePrefix](op, "prefix") match
      case None => base
      case Some(prefix) =>
        val overloadedOnly =
          OpReflect.booleanAnnotationArg[mrpc.annotation.rpcNamePrefix](op, "overloadedOnly").getOrElse(false)
        if !overloadedOnly || overloaded then prefix + base else base

  /**
   * Deterministic, reorder-stable suffix for an overloaded method: `_` followed by a positive
   * hexadecimal hash of the flattened parameter type `show` strings joined by a separator. Distinct
   * signatures yield distinct suffixes; reordering the overloads does not change any one suffix.
   */
  private def overloadSuffix(op: Type[?])(using Quotes): String =
    import quotes.reflect.*
    val sig = OpReflect
      .inputElems(op)
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
