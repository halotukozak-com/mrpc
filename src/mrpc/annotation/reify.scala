package halotukozak.mrpc.annotation

import halotukozak.made.annotation.MetaAnnotation

import scala.quoted.{Expr, FromExpr, Quotes}

/**
 * Steers how the metadata materializer fills a metadata-class constructor parameter with the
 * RPC element's NAME.
 *
 *   - `useRawName = false` (default): maps to made `label` — the source name of the method/param.
 *   - `useRawName = true`: maps to the RESOLVED name (`RpcName.computeAll`), i.e. honoring `@rpcName`.
 *
 * Read by reflection on the metadata class's constructor params — NOT captured in
 * `halotukozak.made.Done.Metadata`, so this is a plain [[scala.annotation.StaticAnnotation]] (not a
 * `halotukozak.made.annotation.MetaAnnotation`). Mirrors commons `reifyName`.
 */
final class reifyName(val useRawName: Boolean) extends MetaAnnotation:
  def this() = this(false)

private[mrpc] object reifyName:
  given FromExpr[reifyName]:
    override def unapply(x: Expr[reifyName])(using Quotes): Option[reifyName] = x match
      case '{ new `reifyName`(${ Expr(useRawName) }) } =>
        Some(new reifyName(useRawName))
      case '{ new `reifyName`() } => Some(new reifyName(false))
      case _ => None

/**
 * Steers a metadata-class constructor parameter to hold the real annotation instance(s) of the RPC
 * element, resolved via made `getAnnotation[A]` / `getAnnotations[A]`. Arity-sensitive: a single
 * `Option[A]`/`A` slot reads one annotation; a collection slot reads all matching annotations.
 *
 * Read by reflection on the metadata class's constructor params, so this is a plain
 * [[scala.annotation.StaticAnnotation]]. Mirrors commons `reifyAnnot`.
 */
final class reifyAnnot extends MetaAnnotation

/**
 * Steers a `Boolean` metadata param to hold whether the RPC element carries an annotation of type `A`
 * — a plain yes/no, unlike `@reifyAnnot`'s single/optional/multi extraction. Mirrors commons `isAnnotated[A]`.
 */
final class isAnnotated[A] extends MetaAnnotation

/** Steers an `Int` metadata param to hold the current method's parameter-list count. Method-level only. */
final class reifyParamListCount extends MetaAnnotation
