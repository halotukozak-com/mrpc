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

  /**
   * Whether a parameter type is the abstract `Raw` carrier. In this standalone matcher `Raw` is not a
   * concrete in-scope type, so no fixture param matches; the check exists so the `@verbatim` branch is
   * faithful (verbatim only when the param IS `Raw`) without ever firing on encoded leaf types.
   */
  def isRawCarrier[T: Type](using Quotes): Boolean =
    import quotes.reflect.*
    // An abstract type member / type parameter (no concrete dealias) is the only thing that could be
    // the engine's `Raw`. Concrete leaf types (Int, String, User, ...) are always encoded. Whether a
    // type is abstract vs. concrete isn't a named member to pattern-match on, so this stays reflect-API.
    TypeRepr.of[T].typeSymbol.isAbstractType

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
