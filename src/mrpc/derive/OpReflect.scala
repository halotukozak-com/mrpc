package mrpc.derive

import made.DoneOperation

import scala.quoted.*

/**
 * One parameter's label, declared type, and raw per-param `Metadata` — a refined `Param` type,
 * mirroring made's own `InputElem` shape, rather than a value: it never leaves this package other than
 * to feed [[Matcher]]/[[MetadataDerivation]]'s own macro-time classification, so it never needs to
 * exist as a runtime value. Read back via `metadataEntries`, or directly via a
 * `case '[Param { type ParamType = t }] => ...` quote pattern at the (few) call sites that only need
 * one field once, same as [[OpPlan]] is read back via local quote-pattern matches in [[Matcher]] /
 * [[AsRawDerivation]] / [[AsRealDerivation]].
 */
private[mrpc] sealed trait Param:
  type Label <: String
  type ParamType
  type Metadata <: Tuple

/**
 * Reflection helpers that read the type-level members of a refined `DoneOperation` (and its
 * `InputElem`s, projected into [[Param]]) — `Label`, `OutputType`, `InputElems`, and the method/param
 * `Metadata` chains.
 *
 * Member reads (`labelOf`, `outputType`, ...) go through plain quote-pattern matching on a structural
 * refinement (`case '[DoneOperation { type Label = l }] => Type.of[l]`) — ordinary quotes/splices, no
 * `quotes.reflect` symbol lookup by name. Only annotation-CONTENT reading (`findAnnotation` and below)
 * still needs `quotes.reflect`: an annotation's constructor arguments are term-level data (mirrors
 * made's `getAnnotationImpl`, extensions.scala), which has no quote-pattern shortcut.
 *
 * Widened to `private[mrpc]` because it is the shared reflection reused by BOTH the engine and
 * metadata materialization — one Done-walk path, no fork.
 */
private[mrpc] object OpReflect:
  /** The op's (or param's) `Metadata` tuple entries as types (each an `AnnotatedType(Meta, annot)`). */
  def metadataEntries(t: Type[? <: { type Metadata <: Tuple }])(using Quotes): List[Type[?]] = t match
    case '[type meta <: Tuple; { type Metadata = meta }] => TupleTraverse.traverseTuple[meta, made.Meta]
    case _ => Nil

  /** `true` when the op carries an annotation of type `A`. */
  def hasAnnotation[A: {Type, FromExpr}](opType: Type[? <: DoneOperation])(using Quotes): Boolean =
    findAnnotation[A](opType).isDefined

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
  def findAnnotation[A: {Type, FromExpr}](using q: Quotes)(opType: Type[? <: DoneOperation]): Option[A] =
    import q.reflect.*
    metadataEntries(opType).iterator
      .map(t => TypeRepr.of(using t))
      .collectFirst:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A].valueOrAbort
