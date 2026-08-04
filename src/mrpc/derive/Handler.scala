package mrpc
package derive

import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

opaque type Handler[Raw, Plan <: OpPlan] = EmptyHandler[Raw, Plan] | NonEmptyHandler[Raw, Plan]

opaque type EmptyHandler[Raw, Plan <: OpPlan] = () => Any
opaque type NonEmptyHandler[Raw, Plan <: OpPlan] = ArgsOf[Plan] => Any

type ArgsOf[Plan <: OpPlan] = Plan match
  case ([args0] =>> OpPlan { type Args = args0 })[args] => args

object Handler:

  /**
   * Named (not anonymous) so callers that already hold a concrete `Plan` — [[AsRealDerivation]]'s
   * generated code, one `p` per classified [[OpPlan]] — can reference it DIRECTLY (`Handler.derived[Raw,
   * p]`) instead of going through `summon`/`summonInline[ExecutionContext => Handler[Raw, p]]`. Ordinary
   * implicit search silently REJECTS this `given` as a candidate for a structurally-refined `Plan`
   * (never even reaching this body — confirmed by direct comparison, not merely a swallowed-exception
   * guess), while an explicit direct reference resolves it correctly; only the `Raw: RawRpc` context
   * bound still needs — and gets — ordinary implicit resolution at the (direct) call site.
   */
  inline def materialize[Raw: RawRpc, Plan <: OpPlan](using ExecutionContext): Handler[Raw, Plan] =
    inline compiletime.erasedValue[Plan] match
      case plan =>
        inline compiletime.erasedValue[plan.OpType] match
          case op =>
            inline compiletime.erasedValue[plan.Args] match
              case _: EmptyTuple =>
                () => handlerBody[Raw, Plan, plan.Args, plan.RpcName, op.ParamLists](EmptyTuple.asInstanceOf[plan.Args])
              case _ =>
                (args: ArgsOf[Plan]) =>
                  handlerBody[Raw, Plan, plan.Args, plan.RpcName, op.ParamLists](args.asInstanceOf[plan.Args])

  inline def handlerBody[Raw: RawRpc as raw, Plan <: OpPlan, Args <: Tuple, Name <: String, Lists <: Tuple](
    tup: Args,
  )(using ec: ExecutionContext,
  ) =
    ${ handlerBodyImpl[Raw, Plan, Args, Name, Lists]('tup, 'raw, 'ec) }

  private def handlerBodyImpl[
    Raw: Type,
    Plan <: OpPlan: Type,
    Args <: Tuple: Type,
    Name <: String: Type,
    Lists <: Tuple: Type,
  ](
    args: Expr[? <: Tuple],
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[?] =
    /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
    def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
      sizes
        .foldLeft((remaining = items, acc = List.empty[List[A]])) { case ((remaining, acc), n) =>
          val (group, rest) = remaining.splitAt(n)
          (rest, group :: acc)
        }
        .acc
        .reverse

    // Recover positional argument terms from the args tuple, each cast to its exact declared type, so
    // the per-param `AsRaw[Raw, t]` encoder applies as the source would. `Args` carries no `Product`
    // bound, so `args` is cast to `Product` here — safe, since every `Args` this is called with
    // (`EmptyTuple` or a `NamedTuple`) IS one at runtime.
    val flatArgTerms: List[Expr[?]] = TupleTraverse.traverseTuple[Args, Any].zipWithIndex.map {
      case ('[t], i) => '{ $args(${ Expr(i) }).asInstanceOf[t] }
      case (_, _) => ???
    }

    val invocation =
      val encodedArgs: List[Expr[Raw]] = flatArgTerms.map { case '{ $arg: arg } =>
        '{ scala.compiletime.summonInline[AsRaw[Raw, arg]].asRaw($arg) }
      }

      val sizes = Type.valueOfTuple[Lists].get.toList.asInstanceOf[List[Int]]
      val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
      val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
      val argsExpr: Expr[List[List[Raw]]] = Expr.ofList(nestedExprs)

      '{ RawInvocation[Raw](compiletime.constValue[Name], $argsExpr) }

    Type.of[Plan] match
      case '[{ type ArityInfo = ArityTag.Fire }] =>
        '{ $raw.fire($invocation) }
      case '[{ type ArityInfo = ArityTag.Call[r] }] =>
        '{
          val futureDecoder: AsReal[Future[Raw], Future[r]] =
            AsReal.forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], $ec)
          futureDecoder.asReal($raw.call($invocation))
        }
      case '[{ type ArityInfo = ArityTag.Get[sub] }] =>
        '{
          val subProxy = compiletime.summonInline[AsReal[RawRpc[Raw], sub]]
          AsReal.makeLazy[RawRpc[Raw], sub](subProxy).asReal($raw.get($invocation))
        }
