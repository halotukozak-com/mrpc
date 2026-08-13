package halotukozak.mrpc.annotation

import made.annotation.MetaAnnotation

/**
 * Steers a metadata-class constructor parameter to be materialized by RECURSING the
 * metadata-class construction on the parameter's type, against the SAME real-symbol context as the
 * enclosing slot (no new RPC element is introduced — the composite groups several metadata reads
 * of one element into a nested class).
 *
 * Read by reflection on the metadata class's constructor params — NOT captured in
 * `made.Done.Metadata`, so this is a plain [[scala.annotation.StaticAnnotation]]. Mirrors commons
 * `composite`.
 */
final class composite extends MetaAnnotation
