package mrpc.derive

import scala.quoted.*

/**
 * Reads the type-level members of a refined [[OpPlan]] (and its [[ParamPlan]]s) — `Label`,
 * `RpcName`, `ArityInfo`, `Params`, `ResultEncoding`, `OpType` — the plan-side counterpart of
 * [[OpReflect]] (which does the same for made's `DoneOperation`). Ad hoc accessors on a `Type[?]`,
 * mirroring [[OpReflect]]'s own shape, rather than one eager bundle: `AsRawDerivation`/
 * `AsRealDerivation` keep carrying the plan's `Type[?]` itself and query it where needed.
 *
 * Match-type extraction (e.g. `type ExtractLabel[P] = P match { case OpPlan { type Label = l } => l
 * }`) does NOT reduce once a type has passed through an opaque `Type[?]` — the same wall [[OpReflect]]
 * already routes around for `DoneOperation` (see made's `MappedTupleEvidenceTest`). Every [[OpPlan]]
 * `Matcher.planAll` builds crosses exactly that boundary (folded into a `Tuple` type, then traversed
 * back into a `List[Type[?]]`), so member lookup here goes through `quotes.reflect`
 * (`typeMember`/`select`/`dealias`) instead, same as [[OpReflect]].
 */
private[derive] object PlanReflect:

  /** One parameter's plan, read back off a [[ParamPlan]] type (mirrors [[OpReflect.Param]]). */
  final case class Param(label: String, paramType: Type[?], encoding: Type[?])

  /** The plan's `Label` member (the underlying op's source name). */
  def labelOf(planType: Type[?])(using Quotes): String = stringMember(planType, "Label")

  /** The plan's `RpcName` member (the resolved rpcName). */
  def rpcNameOf(planType: Type[?])(using Quotes): String = stringMember(planType, "RpcName")

  /** The plan's `ArityInfo` member, as its raw [[ArityTag]] type — quote-pattern-match it directly
    * (`case '[ArityTag.Fire] => ...` / `case '[ArityTag.CallOf[r]] => ...`) at the call site. */
  def arityOf(planType: Type[?])(using Quotes): Type[?] = memberType(planType, "ArityInfo").asType

  /** The plan's `Params` member, traversed and each element projected into a [[Param]]. */
  def paramsOf(planType: Type[?])(using Quotes): List[Param] =
    memberType(planType, "Params").asType match
      case '[type ps <: Tuple; ps] => TupleTraverse.traverseTuple(Type.of[ps]).map(paramOf)
      case _ => Nil

  /** The plan's `ResultEncoding` member, as its raw [[EncodingTag]] type. */
  def resultEncodingOf(planType: Type[?])(using Quotes): Type[?] = memberType(planType, "ResultEncoding").asType

  /** The plan's `OpType` member — the underlying refined `DoneOperation` type it describes. */
  def opTypeOf(planType: Type[?])(using Quotes): Type[?] = memberType(planType, "OpType").asType

  private def paramOf(paramType: Type[?])(using Quotes): Param =
    Param(
      stringMember(paramType, "Label"),
      memberType(paramType, "ParamType").asType,
      memberType(paramType, "Encoding").asType,
    )

  private def memberType(using q: Quotes)(tpe: Type[?], name: String): q.reflect.TypeRepr =
    import q.reflect.*
    val repr = TypeRepr.of(using tpe)
    repr.select(repr.typeSymbol.typeMember(name)).dealias

  private def stringMember(using Quotes)(tpe: Type[?], name: String): String =
    import quotes.reflect.*
    memberType(tpe, name) match
      case ConstantType(StringConstant(s)) => s
      case other => report.errorAndAbort(s"$name is not a string literal: ${other.show}")
