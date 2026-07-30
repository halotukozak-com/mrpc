package mrpc.derive

import made.{Done, DoneOperation, InputElem}
import mrpc.derive.OpReflect.paramOf

import scala.quoted.*

/**
 * Whole-trait resolved RPC names lifted to the type level: `RpcNames[T].Names` is a tuple of singleton
 * string types — one per operation, in `Done.Operations` order — equal to [[RpcName.computeAll]]'s
 * result. `names` is the same tuple as a runtime value.
 *
 * Deliberately NOT a per-op typeclass derived independently for each operation and folded via
 * `compiletime.summonAll[Tuple.Map[Done.Operations, RpcName]]`: that shape pushes each op through
 * `Tuple.Map`'s per-element type-variable inference, which drops any `MetaAnnotation` the op carries
 * (e.g. `@multi`) — annotations don't survive being abstracted into a generic type variable. Instead
 * `RpcNames` walks `Done.Operations` reflectively in ONE PASS (annotations stay intact in `TypeRepr`),
 * reads `@rpcName`/`@rpcNamePrefix` terms and computes overload suffixes value-side via
 * [[RpcName.computeAll]], then EMITS the resolved names as types — which is exactly what
 * `Tuple.Map`/match types cannot do. **Annotation-proof** by construction.
 */
sealed trait RpcNames[T]:
  type Names <: Tuple
  def names: Names

object RpcNames:
  transparent inline given derived[T: Done.Of as done]: RpcNames[T] = ${ deriveImpl[T]('done) }

  /**
   * Reads the resolved names back off `RpcNames[T].Names` (the type-level singletons) as a `List`,
   * in `Done.Operations` order. The single name-resolution authority for macro consumers (engine,
   * metadata): the type-level names lowered to values, rather than each caller re-running
   * `RpcName.computeAll`. The mirror is summoned once via [[summonNames]] and threaded in.
   */
  private[derive] def namesOf[T: Type](rpcNames: Expr[RpcNames[T]])(using Quotes): List[Type[? <: String]] =
    rpcNames.runtimeChecked match
      case '{ type ns <: Tuple; $_ : RpcNames[T] { type Names = ns } } => TupleTraverse.traverseTuple[ns, String]

  private def deriveImpl[T: Type](done: Expr[Done.Of[T]])(using Quotes): Expr[RpcNames[T]] =
    import quotes.reflect.*
    val ops = done match
      case '{ type operations <: Tuple; $_ : { type Operations = operations } } =>
        TupleTraverse.traverseTuple[operations, DoneOperation]

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

    val labels = ops.map(OpReflect.labelOf)

    detectDuplicates(labels, resolved)

    // Fold the resolved names into a singleton-string tuple type `n1 *: n2 *: ... *: EmptyTuple`.
    val namesTupleType: TypeRepr = resolved.foldRight(TypeRepr.of[EmptyTuple]) { (n, acc) =>
      (ConstantType(StringConstant(n)).asType, acc.asType) match
        case ('[type s <: String; s], '[type r <: Tuple; r]) => TypeRepr.of[s *: r]
        case _ => acc
    }

    namesTupleType.asType.runtimeChecked match
      case '[type ns <: Tuple; ns] =>
        val namesValue: Expr[Tuple] = Expr.ofRefinedTuple(resolved.map(Expr(_)))
        '{
          (new RpcNames[T]:
            type Names = ns
            def names: Names = $namesValue.asInstanceOf[ns]
          ): RpcNames[T] { type Names = ns }
        }

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
    val sig = op match
      case '[type elems <: Tuple; DoneOperation { type InputElems = elems }] =>
        TupleTraverse
          .traverseTuple[elems, InputElem]
          .map(paramOf)
          .map:
            case '[Param { type ParamType = t }] => Type.show[t]
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
