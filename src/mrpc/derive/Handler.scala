package halotukozak.mrpc
package derive

import halotukozak.mrpc.conv.{AsRaw, AsReal}
import halotukozak.mrpc.raw.{RawInvocation, RawRpc}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

opaque type Handler[Raw, Plan <: OpPlan] = EmptyHandler[Raw, Plan] | NonEmptyHandler[Raw, Plan]

opaque type EmptyHandler[Raw, Plan <: OpPlan] = () => Any
opaque type NonEmptyHandler[Raw, Plan <: OpPlan] = ArgsOf[Plan] => Any

type ArgsOf[Plan] <: Tuple = Plan match
  case ([args0] =>> OpPlan { type Args = args0 })[args] => args & Tuple

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
        inline compiletime.erasedValue[plan.Args] match
          case _: EmptyTuple =>
            () => handlerBody[Raw, Plan, plan.Args, plan.RpcName, plan.ParamLists](EmptyTuple.asInstanceOf[plan.Args])
          case _ =>
            (args: ArgsOf[Plan]) =>
              handlerBody[Raw, Plan, plan.Args, plan.RpcName, plan.ParamLists](args.asInstanceOf[plan.Args])

  inline private def handlerBody[Raw: RawRpc as raw, Plan <: OpPlan, Args <: Tuple, Name <: String, Lists <: Tuple](
    tup: Args,
  )(using ec: ExecutionContext,
  ): Any =
    val invocation = RawInvocation[Raw](
      compiletime.constValue[Name],
      splitBySizes(encodeArgs[Raw](tup), compiletime.constValueTuple[Lists].toList.asInstanceOf[List[Int]]),
    )
    inline compiletime.erasedValue[Plan] match
      case plan =>
        inline compiletime.erasedValue[plan.ArityInfo] match
          case ArityTag.Fire => raw.fire(invocation)
          case _: ArityTag.Call[r] =>
            AsReal
              .forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], ec)
              .asReal(raw.call(invocation))
          case _: ArityTag.Get[sub] =>
            AsReal
              .makeLazy[RawRpc[Raw], sub](scala.compiletime.summonInline[AsReal[RawRpc[Raw], sub]])
              .asReal(raw.get(invocation))

  /** Encodes each element of a plain (non-`NamedTuple`) args tuple to `Raw` via its own summoned `AsRaw[Raw, h]`. */
  inline private def encodeArgs[Raw](tup: Tuple): List[Raw] = inline tup match
    case _: EmptyTuple => Nil
    case _: (head *: tail) =>
      val head = tup.head.asInstanceOf[head]
      val tail = tup.tail.asInstanceOf[tail]
      scala.compiletime.summonInline[AsRaw[Raw, head]].asRaw(head) :: encodeArgs[Raw](tail)

  private def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
    @tailrec def loop(sizes: List[Int], remaining: List[A], acc: Vector[List[A]]): Vector[List[A]] = sizes match
      case Nil => acc
      case n :: next =>
        val (group, rest) = remaining.splitAt(n)
        loop(next, rest, acc :+ group)

    loop(sizes, items, Vector.empty).toList
