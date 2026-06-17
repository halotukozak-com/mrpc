package mrpc.derive

import scala.quoted.*

/**
 * Direct, compile-time introspection of a real RPC trait — the self-contained, commons-style
 * replacement for the former `made.Done` walk. Instead of summoning a reflection mirror and reading
 * its refined type members, this reads the trait's own method symbols straight off `quotes.reflect`:
 * member name (`Label`), result type (`OutputType`), parameter lists (`InputElems`/`ParamLists`), and
 * `MetaAnnotation`s (read from `sym.annotations`, not a captured `Metadata` tuple).
 *
 * Member selection mirrors commons (and made's old `derivedImpl`) so the SAME operations are picked:
 * user-declared `def`/`val`s, in declaration order, excluding constructors, synthetic/artifact
 * members, and the universal/Product/Equals/Enum surface. Keeping the filter identical preserves v1
 * behavior parity.
 *
 * Widened to `private[mrpc]`: shared by the engine (matching/dispatch) and metadata materialization —
 * one introspection path, no fork.
 */
private[mrpc] object OpReflect:

  /**
   * A single parameter, read off the method's parameter symbol. Fully portable (no `quotes.reflect`
   * payload): label, declared type, and the derived `hasVerbatim` fact. Raw annotation terms (for
   * metadata reification) are read separately via [[paramAnnotations]] within the consumer's scope.
   */
  final case class Param(
    label: String,
    tpe: Type[?],
    hasVerbatim: Boolean,
  )

  private def excludedOwners(using Quotes): Set[quotes.reflect.Symbol] =
    import quotes.reflect.*
    Set(
      defn.AnyClass,
      defn.AnyValClass,
      TypeRepr.of[Object].typeSymbol,
      TypeRepr.of[Product].typeSymbol,
      TypeRepr.of[Equals].typeSymbol,
      TypeRepr.of[scala.reflect.Enum].typeSymbol,
    )

  /**
   * The trait's RPC operation member symbols, in declaration order — the commons/made-faithful filter.
   * This replaces summoning `Done.Of[T]` and traversing `Done.Operations`.
   */
  def operationMembers[T: Type](using Quotes): List[quotes.reflect.Symbol] =
    import quotes.reflect.*
    val tSymbol = TypeRepr.of[T].typeSymbol
    val excluded = excludedOwners
    (tSymbol.fieldMembers ++ tSymbol.methodMembers).distinct
      .sortBy(_.pos.fold(Int.MaxValue)(_.start)) // source declaration order (Position has no Ordering)
      .filter(m => m.isDefDef || m.isValDef)
      .filterNot(_.isClassConstructor)
      .filterNot(m => m.flags.is(Flags.Synthetic) || m.flags.is(Flags.Artifact))
      .filterNot(m => excluded.contains(m.owner))

  /** The member name — the operation's `Label`. */
  def labelOf(using Quotes)(member: quotes.reflect.Symbol): String = member.name

  /** The innermost (post-curry) result type of the member — the operation's `OutputType`. */
  def outputType[T: Type](using Quotes)(member: quotes.reflect.Symbol): Type[?] =
    import quotes.reflect.*
    def innermost(tpe: TypeRepr): TypeRepr = tpe match
      case mt: MethodType => innermost(mt.resType)
      case ByNameType(u) => u
      case other => other
    innermost(TypeRepr.of[T].memberType(member).widen).asType

  /**
   * Per-parameter-list arities (term params only): `Nil` for a no-parens `def`/`val`, `List(0)` for
   * empty-parens `def f()`, `List(1, 2)` for `def f(a)(b, c)`. The client proxy uses these to split a
   * flat encoded-arg list back into the nested `List[List[Raw]]` shape `RawInvocation` expects.
   */
  def paramListSizes(using Quotes)(member: quotes.reflect.Symbol): List[Int] =
    member.paramSymss.filterNot(_.exists(_.isType)).map(_.size)

  /** The member's flattened parameters (across all param lists), each projected into a [[Param]]. */
  def params[T: Type](using Quotes)(member: quotes.reflect.Symbol): List[Param] =
    import quotes.reflect.*
    val realTpe = TypeRepr.of[T]
    val paramSyms = member.paramSymss.flatten.filterNot(_.isType)
    val paramTypes = flatParamTypes(realTpe.memberType(member).widen)
    paramSyms.zip(paramTypes).map { case (sym, tpe) =>
      val hasVerbatim = sym.annotations.exists(a => a.tpe <:< TypeRepr.of[mrpc.annotation.verbatim])
      Param(sym.name, tpe.asType, hasVerbatim)
    }

  /** Per-parameter `MetaAnnotation` terms, in flattened declaration order (for metadata reification). */
  def paramAnnotations(using Quotes)(member: quotes.reflect.Symbol): List[List[quotes.reflect.Term]] =
    member.paramSymss.flatten.filterNot(_.isType).map(_.annotations.filter(isMetaAnnotation))

  /** The member-level `MetaAnnotation` terms (for rpcName/prefix/arity/tag/metadata reading). */
  def methodAnnotations(using Quotes)(member: quotes.reflect.Symbol): List[quotes.reflect.Term] =
    member.annotations.filter(isMetaAnnotation)

  // --- annotation reading (operates on the Term lists above; commons-style, no Done.Metadata) ---

  /** Locates an annotation term of type `A` among `anns`. */
  def findAnnotation[A: Type](using q: Quotes)(anns: List[q.reflect.Term]): Option[q.reflect.Term] =
    import q.reflect.*
    anns.find(_.tpe <:< TypeRepr.of[A])

  def hasAnnotation[A: Type](using q: Quotes)(anns: List[q.reflect.Term]): Boolean =
    findAnnotation[A](anns).isDefined

  def stringAnnotationArg[A: Type](using q: Quotes)(anns: List[q.reflect.Term], field: String): Option[String] =
    findAnnotation[A](anns).flatMap(annot => extractStringArg(annot, field))

  def booleanAnnotationArg[A: Type](using q: Quotes)(anns: List[q.reflect.Term], field: String): Option[Boolean] =
    findAnnotation[A](anns).map(annot => extractBooleanArg(annot, field))

  /** Whether a parameter type is the abstract `Raw` carrier (only thing `@verbatim` may keep verbatim). */
  def isRawCarrier(tpe: Type[?])(using Quotes): Boolean =
    import quotes.reflect.*
    TypeRepr.of(using tpe).typeSymbol.isAbstractType

  // --- internals ---

  private def isMetaAnnotation(using q: Quotes)(annot: q.reflect.Term): Boolean =
    import q.reflect.*
    annot.tpe <:< TypeRepr.of[mrpc.annotation.MetaAnnotation]

  private def flatParamTypes(using q: Quotes)(tpe: q.reflect.TypeRepr): List[q.reflect.TypeRepr] =
    import q.reflect.*
    tpe match
      case MethodType(_, ps, res) => ps ++ flatParamTypes(res)
      case _ => Nil

  private def extractStringArg(using q: Quotes)(annot: q.reflect.Term, field: String): Option[String] =
    import q.reflect.*
    constructorArgs(annot).get(field).collect { case Literal(StringConstant(s)) => s }

  private def extractBooleanArg(using q: Quotes)(annot: q.reflect.Term, field: String): Boolean =
    import q.reflect.*
    constructorArgs(annot).get(field) match
      case Some(Literal(BooleanConstant(b))) => b
      case _ => false

  /** Maps an annotation's primary-constructor parameter names to applied argument terms. */
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
    val named = args.collect { case NamedArg(name, value) => name -> value }.toMap
    val positional = ctorParamNames.zip(args).collect {
      case (name, arg) =>
        arg match
          case _: NamedArg => None
          case other => Some(name -> other)
    }.flatten.toMap
    positional ++ named
