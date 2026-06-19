package mrpc.derive

import scala.quoted.*

/**
 * Whole-trait resolved RPC names lifted to the type level: `RpcNames[T].Names` is a tuple of singleton
 * string types — one per operation, in `Done.Operations` order — equal to [[RpcName.computeAll]]'s
 * result. `names` is the same tuple as a runtime value.
 *
 * Unlike the per-op [[RpcName]] + `summonAll`, this derivation is **annotation-proof**: it walks
 * `Done.Operations` reflectively (annotations stay intact in `TypeRepr`) and runs the cross-op
 * resolution in one pass, so there is no per-op type-variable inference to drop a `MetaAnnotation`
 * (the wall that defeats `summonAll[Tuple.Map[Operations, RpcName]]` on annotated ops like `@multi`).
 * It reads `@rpcName`/`@rpcNamePrefix` terms and computes overload suffixes value-side, then EMITS the
 * resolved names as types — which is exactly what `Tuple.Map`/match types cannot do.
 */
sealed trait RpcNames[T]:
  type Names <: Tuple
  def names: Names

object RpcNames:
  transparent inline given derived[T]: RpcNames[T] = ${ deriveImpl[T] }

  private def deriveImpl[T: Type](using Quotes): Expr[RpcNames[T]] =
    import quotes.reflect.*
    val done = Matcher.summonDone[T]
    val resolved: List[String] = RpcName.computeAll(Matcher.operationTypes[T](done))

    // Fold the resolved names into a singleton-string tuple type `n1 *: n2 *: ... *: EmptyTuple`.
    val namesTupleType: TypeRepr = resolved.foldRight(TypeRepr.of[EmptyTuple]) { (n, acc) =>
      (ConstantType(StringConstant(n)).asType, acc.asType) match
        case ('[type s <: String; s], '[type r <: Tuple; r]) => TypeRepr.of[s *: r]
        case _ => acc
    }

    namesTupleType.asType match
      case '[type ns <: Tuple; ns] =>
        val namesValue: Expr[Tuple] = Expr.ofTupleFromSeq(resolved.map(Expr(_)))
        '{
          (new RpcNames[T]:
            type Names = ns
            def names: Names = $namesValue.asInstanceOf[ns]
          ): RpcNames[T] { type Names = ns }
        }
      case _ => report.errorAndAbort("resolved names did not form a Tuple type")
