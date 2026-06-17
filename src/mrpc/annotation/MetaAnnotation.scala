package mrpc.annotation

import scala.annotation.StaticAnnotation

/**
 * Base for all mrpc RPC annotations. Commons-faithful: a plain `StaticAnnotation` read directly off
 * the trait's method/parameter symbols at compile time (`sym.annotations`), NOT a type-refining
 * annotation captured into a reflection mirror. This is the self-contained replacement for the former
 * `made.annotation.MetaAnnotation` — mrpc no longer depends on made.
 */
trait MetaAnnotation extends StaticAnnotation
