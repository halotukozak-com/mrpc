package mrpc.derive

import scala.quoted.*

/**
 * Reads the type-level members of a refined [[OpPlan]] (and its [[ParamPlan]]s) — `Label`,
 * `RpcName`, `ArityInfo`, `Params`, `ResultEncoding`, `OpType` — the plan-side counterpart of
 * [[OpReflect]] (which does the same for made's `DoneOperation`).
 *
 * Member reads go through plain quote-pattern matching on a structural refinement (`case '[OpPlan {
 * type Label = l }] => Type.of[l]`) — ordinary quotes/splices, no `quotes.reflect` symbol lookup by
 * name. Ad hoc accessors on a `Type[?]`, mirroring [[OpReflect]]'s own shape, rather than one eager
 * bundle: `AsRawDerivation`/`AsRealDerivation` keep carrying the plan's `Type[?]` itself and query it
 * where needed.
 */
private[derive] object PlanReflect:

  /** One parameter's plan, read back off a [[ParamPlan]] type (mirrors [[OpReflect.Param]]). */
  final case class Param(label: String, paramType: Type[?], encoding: Type[?])

  /** The plan's `Label` member (the underlying op's source name). */
  def labelOf(planType: Type[?])(using Quotes): String =
    labelTypeOf(planType) match
      case '[type l <: String; l] =>
        Type.valueOfConstant[l].getOrElse(quotes.reflect.report.errorAndAbort("Label is not a string literal")).toString

  /** The plan's `Label` member as a type (the singleton string type itself). */
  def labelTypeOf(planType: Type[?])(using Quotes): Type[?] =
    planType match
      case '[OpPlan { type Label = l }] => Type.of[l]
      case '[ParamPlan { type Label = l }] => Type.of[l]
      case _ => quotes.reflect.report.errorAndAbort(s"no Label member on ${Type.show(using planType)}")

  /** The plan's `RpcName` member (the resolved rpcName). */
  def rpcNameOf(planType: Type[?])(using Quotes): String =
    planType match
      case '[OpPlan { type RpcName = n }] =>
        Type.of[n] match
          case '[type nn <: String; nn] =>
            Type.valueOfConstant[nn].getOrElse(quotes.reflect.report.errorAndAbort("RpcName is not a string literal")).toString
      case _ => quotes.reflect.report.errorAndAbort(s"no RpcName member on ${Type.show(using planType)}")

  /** The plan's `ArityInfo` member, as its raw [[ArityTag]] type — quote-pattern-match it directly
    * (`case '[ArityTag.Fire] => ...` / `case '[ArityTag.CallOf[r]] => ...`) at the call site. */
  def arityOf(planType: Type[?])(using Quotes): Type[?] =
    planType match
      case '[OpPlan { type ArityInfo = a }] => Type.of[a]
      case _ => quotes.reflect.report.errorAndAbort(s"no ArityInfo member on ${Type.show(using planType)}")

  /** The plan's `Params` member, traversed and each element projected into a [[Param]]. */
  def paramsOf(planType: Type[?])(using Quotes): List[Param] =
    planType match
      case '[OpPlan { type Params = ps }] =>
        Type.of[ps] match
          case '[type pst <: Tuple; pst] => TupleTraverse.traverseTuple(Type.of[pst]).map(paramOf)
      case _ => Nil

  /** The plan's `ResultEncoding` member, as its raw [[EncodingTag]] type. */
  def resultEncodingOf(planType: Type[?])(using Quotes): Type[?] =
    planType match
      case '[OpPlan { type ResultEncoding = e }] => Type.of[e]
      case _ => quotes.reflect.report.errorAndAbort(s"no ResultEncoding member on ${Type.show(using planType)}")

  /** The plan's `OpType` member — the underlying refined `DoneOperation` type it describes. */
  def opTypeOf(planType: Type[?])(using Quotes): Type[?] =
    planType match
      case '[OpPlan { type OpType = o }] => Type.of[o]
      case _ => quotes.reflect.report.errorAndAbort(s"no OpType member on ${Type.show(using planType)}")

  private def paramOf(paramType: Type[?])(using Quotes): Param =
    paramType match
      case '[ParamPlan { type Label = l; type ParamType = t; type Encoding = e }] =>
        Param(labelOf(paramType), Type.of[t], Type.of[e])
      case _ => quotes.reflect.report.errorAndAbort(s"could not read ParamPlan for ${Type.show(using paramType)}")
