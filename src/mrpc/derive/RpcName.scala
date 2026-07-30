package mrpc.derive

import scala.quoted.*

/**
 * Per-operation resolved RPC name as a typeclass: `RpcName[Op].name` (and the singleton `type Name`)
 * is the SAME string [[RpcName.computeAll]] resolves for that op. Derived independently per op, so a
 * whole trait's names are `compiletime.summonAll[Tuple.Map[Done.Operations, RpcName]]`.
 *
 * Overload disambiguation stays correct despite being per-op: each `DoneOperation` carries
 * `OuterType` (the enclosing trait), so the derivation re-derives `Done.Of[OuterType]` to see the
 * sibling operations and decide whether this op's base name is overloaded. (Cost: O(n) sibling walk
 * per op; `computeAll` does it once for all — prefer `computeAll` inside the engine macros, this
 * typeclass is for type-level/summonAll consumers.)
 */
sealed trait RpcName[Op]:
  type Name <: String
  def name: Name

object RpcName:
  transparent inline given derived[Op]: RpcName[Op] = ${ deriveImpl[Op] }

  private def deriveImpl[Op: Type](using Quotes): Expr[RpcName[Op]] =
    import quotes.reflect.*
    val opTpe = TypeRepr.of[Op]
    val outerTpe = opTpe.select(opTpe.typeSymbol.typeMember("OuterType")).dealias
    val nm: String = outerTpe.asType match
      case '[real] =>
        val done = Matcher.summonDone[real]
        val siblings = Matcher.operationTypes[real](done)
        resolveOne(Type.of[Op], siblings)
      case _ => report.errorAndAbort(s"cannot read OuterType of ${opTpe.show}")
    ConstantType(StringConstant(nm)).asType match
      case '[type n <: String; n] =>
        '{
          (new RpcName[Op]:
            type Name = n
            def name: Name = ${ Expr(nm) }.asInstanceOf[n]
          ): RpcName[Op] { type Name = n }
        }

  /** The resolved name for `op` given its sibling operations — the per-op slice of [[computeAll]]. */
  private def resolveOne(op: Type[?], siblings: List[Type[?]])(using Quotes): String =
    val base = baseName(op)
    val overloaded = siblings.map(baseName).count(_ == base) > 1
    val prefixed = applyPrefix(op, base, overloaded)
    if overloaded && !OpReflect.hasAnnotation[mrpc.annotation.rpcName](op) then prefixed + overloadSuffix(op)
    else prefixed

  /**
   * Resolves the final rpcName for every op (positionally), aborting on a duplicate. Resolution order
   * mirrors commons:
   *   1. `@rpcName("...")` wins over the method label (made's `@name` already feeds the label).
   *   2. `@rpcNamePrefix(prefix, overloadedOnly)` prepends `prefix` (always, or only on overloads).
   *   3. Overloaded methods get a DETERMINISTIC, REORDER-STABLE, signature-based suffix — never a
   *      positional `_1`/`_2`. Non-overloaded names are left untouched.
   * Any final name shared by ops that are NOT a legitimate overload group is a collision and aborts.
   */
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
      .map { p =>
        val paramType = p.runtimeChecked match
          case '[Param { type ParamType = t }] => Type.of[t]
        TypeRepr.of(using paramType).show
      }
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
