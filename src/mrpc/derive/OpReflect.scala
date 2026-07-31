package mrpc.derive

import made.DoneOperation

import scala.quoted.*

/**
 * One parameter's label, declared type, and raw per-param `Metadata` — a refined `Param` type,
 * mirroring made's own `InputElem` shape, rather than a value: it never leaves this package other than
 * to feed [[Matcher]]/[[MetadataDerivation]]'s own macro-time classification, so it never needs to
 * exist as a runtime value. Read back via `metadataEntries`/`paramHasVerbatim`, or directly via a
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

  /** `true` when the param carries a `@verbatim` annotation. */
  def paramHasVerbatim[m <: Tuple: Type](using Quotes): Boolean =
    TupleTraverse.traverseTuple[m, made.Meta].exists(isAnnotation[mrpc.annotation.verbatim])

  /**
   * The op's per-parameter-list arities, read off its `ParamLists` (a tuple of singleton `Int`s).
   * The client proxy uses these to split a flat encoded-argument list back into the nested
   * `List[List[Raw]]` shape `RawInvocation` expects (the inverse of the server adapter's `flatten`):
   *   - `EmptyTuple`            (no-parens `def f` / `val`)  -> `Nil`
   *   - `0 *: EmptyTuple`       (empty-parens `def f()`)     -> `List(0)`
   *   - `1 *: 2 *: EmptyTuple`  (`def f(a)(b, c)`)           -> `List(1, 2)`
   */
  def paramListSizes(opType: Type[?])(using Quotes): List[Int] =
    opType match
      case '[type lists <: Tuple; DoneOperation { type ParamLists = lists }] =>
        Type.valueOfTuple[lists].get.toList.asInstanceOf[List[Int]]
      case _ => quotes.reflect.report.errorAndAbort(s"no ParamLists member on ${Type.show(using opType)}")

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

  private def isAnnotation[A: Type](entry: Type[?])(using Quotes): Boolean =
    import quotes.reflect.*
    TypeRepr.of(using entry) match
      case AnnotatedType(_, annot) => annot.tpe <:< TypeRepr.of[A]
      case _ => false

  /** Extracts a constant `String` constructor argument named `field` from an annotation term. */
  private[derive] def extractStringArg(using q: Quotes)(annot: q.reflect.Term, field: String): Option[String] =
    import q.reflect.*
    constructorArgs(annot)
      .get(field)
      .collect:
        case Literal(StringConstant(s)) => s

  /** Extracts a constant `Boolean` constructor argument named `field` (default `false`). */
  private[derive] def extractBooleanArg(using q: Quotes)(annot: q.reflect.Term, field: String): Boolean =
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
