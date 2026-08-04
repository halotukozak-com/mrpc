package mrpc.annotation

import made.annotation.MetaAnnotation

/**
 * Steers a metadata-class constructor parameter to be materialized by IMPLICIT SEARCH
 * (`Expr.summon`) for the parameter's declared type. The optional `clue` is surfaced in the
 * error message when the implicit cannot be found.
 *
 * Read by reflection on the metadata class's constructor params — NOT captured in
 * `made.Done.Metadata`, so this is a plain [[scala.annotation.StaticAnnotation]]. Mirrors commons
 * `infer`.
 */
final class infer extends MetaAnnotation