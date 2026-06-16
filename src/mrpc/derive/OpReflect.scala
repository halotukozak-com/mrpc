package mrpc.derive

import scala.quoted.*

/**
 * Reflection helpers that read the type-level members of a refined `DoneOperation` (and its
 * `InputElem`s) — `Label`, `OutputType`, `InputElems`, and the method/param `Metadata` chains.
 *
 * Annotation reading mirrors made's `getAnnotationImpl` (extensions.scala): each `MetaAnnotation` is
 * captured in the `Metadata` tuple as an `AnnotatedType(Meta, annot)`, so a `<:<` match over the
 * traversed metadata entries locates an annotation and exposes it as a term for value extraction.
 *
 * Widened to `private[mrpc]` because it is the shared reflection reused by BOTH the engine and
 * metadata materialization — one Done-walk path, no fork.
 */
private[mrpc] object OpReflect:

  /**
   * A single parameter: its label, declared type, the raw per-param `Metadata` entries (each an
   * `AnnotatedType(Meta, annot)`), and the derived `hasVerbatim` fact. `metadataEntries` is threaded
   * from the same `paramOf` traversal that computes `hasVerbatim`, so the metadata materializer can
   * reify per-param annotations without a second walk.
   */
  final case class Param(
    label: String,
    tpe: Type[?],
    metadataEntries: List[Type[?]],
    hasVerbatim: Boolean,
  )

  /** The op's `Label` singleton-string member as a plain `String`. */
  def labelOf(opType: Type[?])(using Quotes): String =
    import quotes.reflect.*
    memberType(opType, "Label") match
      case ConstantType(StringConstant(s)) => s
      case other => report.errorAndAbort(s"operation Label is not a string literal: ${other.show}")

  /** The op's `OutputType` member as a `Type`. */
  def outputType(opType: Type[?])(using Quotes): Type[?] =
    memberType(opType, "OutputType").asType

  /**
   * The op's per-parameter-list arities, read off its `ParamLists` (a tuple of singleton `Int`s).
   * The client proxy uses these to split a flat encoded-argument list back into the nested
   * `List[List[Raw]]` shape `RawInvocation` expects (the inverse of the server adapter's `flatten`):
   *   - `EmptyTuple`            (no-parens `def f` / `val`)  -> `Nil`
   *   - `0 *: EmptyTuple`       (empty-parens `def f()`)     -> `List(0)`
   *   - `1 *: 2 *: EmptyTuple`  (`def f(a)(b, c)`)           -> `List(1, 2)`
   */
  def paramListSizes(opType: Type[?])(using Quotes): List[Int] =
    import quotes.reflect.*
    memberType(opType, "ParamLists").asType match
      case '[type lists <: Tuple; lists] =>
        TupleTraverse.traverseTuple(Type.of[lists]).map { t =>
          TypeRepr.of(using t).dealias match
            case ConstantType(IntConstant(n)) => n
            case other =>
              report.errorAndAbort(s"ParamLists entry is not an Int literal: ${other.show}")
        }
      case _ => report.errorAndAbort("ParamLists is not a tuple")

  /** The op's flattened `InputElems`, each projected into a [[Param]]. */
  def inputElems(opType: Type[?])(using Quotes): List[Param] =
    import quotes.reflect.*
    memberType(opType, "InputElems").asType match
      case '[type elems <: Tuple; elems] =>
        TupleTraverse.traverseTuple(Type.of[elems]).map(paramOf)
      case _ => report.errorAndAbort("InputElems is not a tuple")

  /** The op's method-level `Metadata` tuple entries as types (each an `AnnotatedType(Meta, annot)`). */
  def metadataEntries(opType: Type[?])(using Quotes): List[Type[?]] =
    memberType(opType, "Metadata").asType match
      case '[type meta <: Tuple; meta] => TupleTraverse.traverseTuple(Type.of[meta])
      case _ => Nil

  /** Reads a singleton-string-valued field `field` off an annotation of type `A` on the op, if present. */
  def stringAnnotationArg[A: Type](opType: Type[?], field: String)(using Quotes): Option[String] =
    findAnnotation[A](opType).flatMap(annot => extractStringArg(annot, field))

  /** Reads a `Boolean` field `field` off an annotation of type `A` on the op (default `false`). */
  def booleanAnnotationArg[A: Type](opType: Type[?], field: String)(using Quotes): Option[Boolean] =
    findAnnotation[A](opType).map(annot => extractBooleanArg(annot, field))

  /** `true` when the op carries an annotation of type `A`. */
  def hasAnnotation[A: Type](opType: Type[?])(using Quotes): Boolean =
    findAnnotation[A](opType).isDefined

  /**
   * Whether a parameter type is the abstract `Raw` carrier. In this standalone matcher `Raw` is not a
   * concrete in-scope type, so no fixture param matches; the check exists so the `@verbatim` branch is
   * faithful (verbatim only when the param IS `Raw`) without ever firing on encoded leaf types.
   */
  def isRawCarrier(tpe: Type[?])(using Quotes): Boolean =
    import quotes.reflect.*
    val repr = TypeRepr.of(using tpe)
    // An abstract type member / type parameter (no concrete dealias) is the only thing that could be
    // the engine's `Raw`. Concrete leaf types (Int, String, User, ...) are always encoded.
    repr.typeSymbol.isAbstractType

  // --- internals ---

  private def memberType(using q: Quotes)(tpe: Type[?], name: String): q.reflect.TypeRepr =
    import q.reflect.*
    val repr = TypeRepr.of(using tpe)
    val member = repr.typeSymbol.typeMember(name)
    repr.select(member).dealias

  private def paramOf(elemType: Type[?])(using Quotes): Param =
    import quotes.reflect.*
    val repr = TypeRepr.of(using elemType)
    val tpe = repr.select(repr.typeSymbol.typeMember("Type")).dealias.asType
    val label = repr.select(repr.typeSymbol.typeMember("Label")).dealias match
      case ConstantType(StringConstant(s)) => s
      case _ => ""
    val metaEntries = repr.select(repr.typeSymbol.typeMember("Metadata")).dealias.asType match
      case '[type meta <: Tuple; meta] => TupleTraverse.traverseTuple(Type.of[meta])
      case _ => Nil
    Param(label, tpe, metaEntries, metaEntries.exists(isAnnotation[mrpc.annotation.verbatim]))

  /** Locates an annotation term of type `A` in the op's `Metadata`, like made's `getAnnotationImpl`. */
  private def findAnnotation[A: Type](using q: Quotes)(opType: Type[?]): Option[q.reflect.Term] =
    import q.reflect.*
    metadataEntries(opType).iterator
      .map(t => TypeRepr.of(using t))
      .collectFirst:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot

  private def isAnnotation[A: Type](entry: Type[?])(using Quotes): Boolean =
    import quotes.reflect.*
    TypeRepr.of(using entry) match
      case AnnotatedType(_, annot) => annot.tpe <:< TypeRepr.of[A]
      case _ => false

  /** Extracts a constant `String` constructor argument named `field` from an annotation term. */
  private def extractStringArg(using q: Quotes)(annot: q.reflect.Term, field: String): Option[String] =
    import q.reflect.*
    constructorArgs(annot)
      .get(field)
      .collect:
        case Literal(StringConstant(s)) => s

  /** Extracts a constant `Boolean` constructor argument named `field` (default `false`). */
  private def extractBooleanArg(using q: Quotes)(annot: q.reflect.Term, field: String): Boolean =
    import q.reflect.*
    constructorArgs(annot).get(field) match
      case Some(Literal(BooleanConstant(b))) => b
      case _ => false

  /**
   * Maps an annotation's primary-constructor parameter names to the applied argument terms. Handles
   * both positional args (zipped against the constructor parameter names in order) and `NamedArg`
   * entries (keyed by their explicit name, overriding the positional mapping). The inner literal is
   * unwrapped from any `NamedArg`.
   */
  private def constructorArgs(using q: Quotes)(annot: q.reflect.Term): Map[String, q.reflect.Term] =
    import q.reflect.*
    val ctorParamNames = annot.tpe.typeSymbol.primaryConstructor.paramSymss.flatten
      .filterNot(_.isType)
      .map(_.name)

    def collectArgs(term: Term): List[Term] = term match
      case Apply(fun, args) => collectArgs(fun) ++ args
      case TypeApply(fun, _) => collectArgs(fun)
      case _ => Nil

    val args = collectArgs(annot)
    val named: Map[String, Term] = args
      .collect:
        case NamedArg(name, value) => name -> value
      .toMap
    val positional: Map[String, Term] = ctorParamNames
      .zip(args)
      .collect:
        case (name, arg) =>
          arg match
            case NamedArg(_, _) => None
            case other => Some(name -> other)
      .flatten
      .toMap
    positional ++ named
