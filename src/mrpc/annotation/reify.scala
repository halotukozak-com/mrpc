package mrpc.annotation

/**
 * Steers how the metadata materializer fills a metadata-class constructor parameter with the
 * RPC element's NAME.
 *
 *   - `useRawName = false` (default): maps to made `label` — the source name of the method/param.
 *   - `useRawName = true`: maps to the RESOLVED name (`RpcName.computeAll`), i.e. honoring `@rpcName`.
 *
 * Read by reflection on the metadata class's constructor params — NOT captured in
 * `made.Done.Metadata`, so this is a plain [[scala.annotation.StaticAnnotation]] (not a
 * `made.annotation.MetaAnnotation`). Mirrors commons `reifyName`.
 */
final class reifyName(val useRawName: Boolean = false) extends scala.annotation.StaticAnnotation

/**
 * Steers a metadata-class constructor parameter to hold the real annotation instance(s) of the RPC
 * element, resolved via made `getAnnotation[A]` / `getAnnotations[A]`. Arity-sensitive: a single
 * `Option[A]`/`A` slot reads one annotation; a collection slot reads all matching annotations.
 *
 * Read by reflection on the metadata class's constructor params, so this is a plain
 * [[scala.annotation.StaticAnnotation]]. Mirrors commons `reifyAnnot`.
 */
final class reifyAnnot extends scala.annotation.StaticAnnotation
