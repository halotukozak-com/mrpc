package mrpc
package derive

import made.DoneOperation
import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

sealed trait Handler[Raw, Op <: OpPlan]

sealed trait EmptyHandler[Raw, Op <: OpPlan] extends Handler[Raw, Op], (() => Any)
sealed trait NonEmptyHandler[Raw, Op <: OpPlan] extends Handler[Raw, Op], ((Op#Args & Tuple) => Any)

object Handler:

  inline given [Raw: RawRpc, Plan <: OpPlan] => ExecutionContext => Handler[Raw, Plan] =
    inline compiletime.erasedValue[Plan] match
      case plan =>
        inline compiletime.erasedValue[plan.OpType] match
          case op: DoneOperation =>
            inline compiletime.erasedValue[plan.Args] match
              case _: EmptyTuple =>
                new EmptyHandler[Raw, Plan]:
                  override def apply(): Any =
                    handlerBody[Raw, Plan, plan.Args, plan.RpcName, op.ParamLists](EmptyTuple.asInstanceOf[plan.Args])
              case _ =>
                new NonEmptyHandler[Raw, Plan]:
                  override def apply(args: Plan#Args & Tuple): Any =
                    handlerBody[Raw, Plan, plan.Args, plan.RpcName, op.ParamLists](args.asInstanceOf[plan.Args])

  inline def handlerBody[Raw: RawRpc as raw, Plan <: OpPlan, Args <: Tuple, Name <: String, Lists <: Tuple](
    tup: Args,
  )(using ec: ExecutionContext,
  ) =
    ${
      handlerBodyImpl[Raw, Plan, Args, Name](
        'tup,
        'raw,
        'ec,
        '{ compiletime.constValueTuple[Lists].toList.asInstanceOf[List[Int]] },
      )
    }

  private def handlerBodyImpl[Raw: Type, Plan <: OpPlan: Type, Args <: Tuple: Type, Name <: String: Type](
    args: Expr[? <: Tuple],
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
    lists: Expr[List[Int]],
  )(using Quotes,
  ): Expr[?] =
    import quotes.reflect.*
    // Recover positional argument terms from the args tuple, each cast to its exact declared type, so
    // the per-param `AsRaw[Raw, t]` encoder applies as the source would. `A` carries no `Product`
    // bound (see `handlerFor`), so `args` is cast to `Product` here — safe, since every `A` this is
    // called with (`EmptyTuple` or a `NamedTuple`) IS one at runtime.
    val flatArgTerms: List[Expr[?]] = TupleTraverse.traverseTuple[Args, Any].zipWithIndex.map { case ('[t], i) =>
      '{ $args(${ Expr(i) }).asInstanceOf[t] }
    }

    val invocation =
      val encodedArgs: List[Expr[Raw]] = flatArgTerms.map { case '{ $arg: arg } =>
        '{ scala.compiletime.summonInline[AsRaw[Raw, arg]].asRaw($arg) }
      }

      val sizes = lists.valueOrAbort
      val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
      val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
      val argsExpr: Expr[List[List[Raw]]] = Expr.ofList(nestedExprs)

      '{ RawInvocation[Raw](compiletime.constValue[Name], $argsExpr) }

    /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
    def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
      sizes
        .foldLeft((remaining = items, acc = List.empty[List[A]])) { case ((remaining, acc), n) =>
          val (group, rest) = remaining.splitAt(n)
          (rest, group :: acc)
        }
        .acc
        .reverse

    Type.of[Plan] match
      case '[{ type ArityInfo = ArityTag.Fire }] =>
        '{ $raw.fire($invocation) }
      case '[{ type ArityInfo = ArityTag.CallOf[r] }] =>
        '{
          val futureDecoder: AsReal[Future[Raw], Future[r]] =
            AsReal.forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], $ec)
          futureDecoder.asReal($raw.call($invocation))
        }
      case '[{ type ArityInfo = ArityTag.GetOf[sub] }] =>
        '{
          val subProxy = compiletime.summonInline[AsReal[RawRpc[Raw], sub]]
          AsReal.makeLazy[RawRpc[Raw], sub](subProxy).asReal($raw.get($invocation))
        }
