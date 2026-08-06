package mrpc.derive

import scala.quoted.*

/**
 * Reflection helpers over a refined `DoneOperation`'s type-level members (`InputElems`, `Metadata`
 * chains) and its annotations. Annotation-CONTENT reading (`findAnnotation` and below) needs
 * `quotes.reflect`: an annotation's constructor arguments are term-level data with no quote-pattern
 * shortcut (mirrors made's `getAnnotationImpl`).
 *
 * `private[mrpc]`: shared by both the engine and metadata materialization, one Done-walk path.
 */
private[mrpc] object OpReflect:

  /** `true` when the op carries an annotation of type `A`. */
  def hasAnnotation[A: {Type, FromExpr}, Metadata <: Tuple: Type](using Quotes): Boolean =
    findAnnotation[A, Metadata].isDefined

  // --- internals ---

  /** Locates an annotation term of type `A` in the op's `Metadata`, like made's `getAnnotationImpl`. */
  def findAnnotation[A: {Type, FromExpr}, Metadata <: Tuple: Type](using q: Quotes): Option[A] =
    import q.reflect.*
    TupleTraverse
      .traverseTuple[Metadata, made.Meta]
      .iterator
      .map(t => TypeRepr.of(using t))
      .collectFirst:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A].valueOrAbort
