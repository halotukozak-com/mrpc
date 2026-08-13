package halotukozak.mrpc.annotation

import made.annotation.MetaAnnotation

/** Single-value arity marker. Mirrors commons `single`. */
final class single extends MetaAnnotation

/** Multi-value (collection) arity marker. Mirrors commons `multi`. */
final class multi extends MetaAnnotation

/**
 * Optional arity marker — the element may be absent. Mirrors commons `optional`. Kept extending
 * `made.annotation.MetaAnnotation` for consistency with `single`/`multi`, so it is captured in
 * `made.Done.Metadata` and queryable via made `getAnnotation`/`hasAnnotation`.
 */
final class optional extends MetaAnnotation
