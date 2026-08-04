package mrpc
package derive

import made.{containsOnly, Done, DoneOperation}
import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}
import mrpc.realCons

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

/**
 * Server-adapter derivation: builds an `AsRaw[RawRpc[Raw], Real]` that turns a real trait instance
 * into a transport-facing [[RawRpc]]. The generated `RawRpc[Raw]` dispatches each incoming
 * [[RawInvocation]] by `rpcName`, decodes every argument to its EXACT declared parameter type via a
 * summoned `AsReal[Raw, paramType]`, calls `made.Done.invoke`, and encodes the result back to `Raw`.
 *
 * Decoding to the exact `InputElem.Type` before tupling is what makes made's internal unboxing cast
 * (in `DoneOperation.apply`) a provable no-op: the value handed to the tuple is already statically
 * the declared type, so no boxing/unboxing mismatch can crash even on primitives or wrapper types.
 *
 * The `match` arms are PARTITIONED BY ARITY — only `fire`-arity ops appear in `fire`, only
 * `call`-arity in `call`, only `get`-arity in `get`. An `rpcName` outside an arity's known set falls
 * through to an explicit rejection (no silent no-op), mirroring commons' unknown-method handling.
 */
object AsRawDerivation:

  /**
   * The server-adapter conversion as a plain value: `asRaw` wraps a `Real` in the `RawRpc[Raw]` built
   * by [[buildRawRpc]]. The `AsRaw` wrapper is ordinary Scala (a SAM lambda); only the `RawRpc` itself
   * — its arity-partitioned `fire`/`call`/`get` dispatch — is generated.
   */
  inline def impl[Raw, Real: Done.Of](plans: Plans[Real])(using ExecutionContext): AsRaw[RawRpc[Raw], Real] =
    (api: Real) => buildRawRpc[Raw, Real, plans.Underlying](api)

  /**
   * Macro entry: assembles the dispatching `RawRpc[Raw]` for a `Real` instance. `Done.Of[Real]` and
   * `Plans[Real]` are summoned ONCE here (via their own context bounds) and shared across the
   * `fire`/`call`/`get` bodies below — unlike three independent macro entries, which would each
   * re-derive them.
   */
  inline private def buildRawRpc[Raw, Real: {Done.Of as done}, Plans <: Tuple](
    api: Real,
  )(using Plans containsOnly OpPlan,
  )(using ec: ExecutionContext,
  ): RawRpc[Raw] =
    type Names = Tuple.Map[
      Plans,
      [op] =>> op match
        case ([n] =>> OpPlan { type RpcName = n })[name] => name,
    ]
    mkRawRpc[Raw](
      fireBody[Raw, Real, Plans, Names](api),
      callBody[Raw, Real, Plans, Names](api),
      getBody[Raw, Real, Plans, Names](api),
    )

  /**
   * `mkRawRpc` is an ordinary, non-`inline` method — an anonymous class defined directly inside an
   * `inline`/macro body is otherwise recompiled at every inline site.
   */
  private def mkRawRpc[Raw](
    fireFn: RawInvocation[Raw] => Unit,
    callFn: RawInvocation[Raw] => Future[Raw],
    getFn: RawInvocation[Raw] => RawRpc[Raw],
  ): RawRpc[Raw] = new:
    def fire(invocation: RawInvocation[Raw]): Unit = fireFn(invocation)
    def call(invocation: RawInvocation[Raw]): Future[Raw] = callFn(invocation)
    def get(invocation: RawInvocation[Raw]): RawRpc[Raw] = getFn(invocation)

  // --- arity-partitioned dispatch bodies ---

  transparent inline private def fireArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  ): Tuple = arms[Raw, Real, Acc, [X] =>> Unit](
    [A <: ArityTag, Op <: OpPlan] =>
      index =>
        inline compiletime.erasedValue[A] match
          case _: ArityTag.Fire =>
            (inv: RawInvocation[Raw]) => invoke[Raw, Real, Any, Op](api, inv, index): Unit
          case _ => (inv: RawInvocation[Raw]) => reject(inv)
    ,
    api,
  )(0)

  /**
   * Each body builds `values` over EVERY entry of `plans` (not just its own arity), positionally
   * aligned with the shared `Names` by construction — an op outside this body's own arity gets a
   * `rejectImpl` thunk in its slot instead of real dispatch logic. This is what keeps `Names`/`values`
   * from silently drifting apart (the bug a per-body-filtered `Names` had): there is no separate
   * filter step to fall out of sync with `values`'s own filter.
   *
   * Every value is a THUNK (`() => Result`), not the invocation itself: `matchFromImpl` builds
   * `case name => args(index)` over the WHOLE `values` tuple, and constructing that tuple at all
   * would otherwise force every op's `invokeOp` — decoding `inv`'s args against every op's own,
   * generally mismatched, parameter types, and running every op's real implementation — on every
   * single dispatch, not just the one op whose name matched. Thunking defers all of that to the
   * final `()` below, which only ever runs the ONE arm `matchFromImpl` actually selected.
   */
  inline private def fireBody[Raw, Real, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  ): Unit = matchFrom(NamedTuple.build[Names]()(fireArms[Raw, Real, Plans](api)))[RawInvocation[Raw] => Unit](
    inv.rpcName,
    reject,
  )(inv)

  transparent inline private def arms[Raw, Real: Done.Of, Acc <: Tuple, F[_ <: Raw]](
    inline f: [A <: ArityTag, Op <: OpPlan] => Int => RawInvocation[Raw] => F[Raw],
    api: Real,
  )(
    index: Int,
  ): Tuple =
    inline compiletime.erasedValue[Acc] match
      case _: EmptyTuple => EmptyTuple
      case _: (h *: t) =>
        val arm = inline compiletime.erasedValue[h] match
          case op: OpPlan =>
            f[op.ArityInfo, op.type](index)
        arms[Raw, Real, t, F](f, api)(index + 1).realCons(arm)

  transparent inline private def callArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  )(using ec: ExecutionContext,
  ): Tuple =
    arms[Raw, Real, Acc, Future](
      [A <: ArityTag, Op <: OpPlan] =>
        index =>
          inline compiletime.erasedValue[A] match
            case _: ArityTag.Call[r] =>
              (inv: RawInvocation[Raw]) =>
                val futureEncoder = AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], ec)
                futureEncoder.asRaw(invoke[Raw, Real, Future[r], Op](api, inv, index))
            case _ => (inv: RawInvocation[Raw]) => reject(inv)
      ,
      api,
    )(0)

  inline private def callBody[Raw, Real: Done.Of, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  )(using ExecutionContext,
  ): Future[Raw] = matchFrom(
    NamedTuple.build[Names]()(callArms[Raw, Real, Plans](api)),
  )[RawInvocation[Raw] => Future[Raw]](inv.rpcName, reject)(inv)

  transparent inline private def bodyArms[Raw, Real: Done.Of, Acc <: Tuple](
    api: Real,
  ): Tuple =
    arms[Raw, Real, Acc, RawRpc](
      [A <: ArityTag, Op <: OpPlan] =>
        index =>
          inline compiletime.erasedValue[A] match
            case _: ArityTag.Get[sub] =>
              (inv: RawInvocation[Raw]) =>
                AsRaw
                  .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
                  .asRaw(invoke[Raw, Real, sub, Op](api, inv, index))
            case _ => (inv: RawInvocation[Raw]) => reject(inv)
      ,
      api,
    )(0)

  inline private def getBody[Raw, Real: Done.Of, Plans <: Tuple, Names <: Tuple](
    api: Real,
  )(
    inv: RawInvocation[Raw],
  ): RawRpc[Raw] =
    matchFrom(
      NamedTuple.build[Names]()(bodyArms[Raw, Real, Plans](api)),
    )[RawInvocation[Raw] => RawRpc[Raw]](inv.rpcName, reject)(inv)

  // --- shared helpers ---

  /** Each parameter's declared `ParamType`, in the op's declaration order, off `plan`'s `Params`. */
  private def paramTypesOf[Plan <: OpPlan: Type](using Quotes): List[Type[?]] =
    Type.of[Plan] match
      case '[type ps <: Tuple; OpPlan { type Params = ps }] =>
        TupleTraverse.traverseTuple[ps, ParamPlan].map { case '[ParamPlan { type ParamType = t }] => Type.of[t] }

  inline private def invoke[Raw, Real: Done.Of as done, R, Plan <: OpPlan](
    api: Real,
    inv: RawInvocation[Raw],
    index: Int,
  ): R = ${ invokeImpl[Raw, Real, R, Plan]('api, 'inv, 'index, 'done) }

  private def invokeImpl[Raw: Type, Real: Type, R: Type, Plan <: OpPlan: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    index: Expr[Int],
    done: Expr[Done.Of[Real]],
  )(using quotes: Quotes,
  ): Expr[R] =
    val flatArgs = '{ ${ inv }.args.flatten }

    val decodedArgs: List[Expr[?]] = paramTypesOf[Plan].zipWithIndex.map:
      case ('[t], i) =>
        '{ scala.compiletime.summonInline[AsReal[Raw, t]].asReal($flatArgs(${ Expr(i) })) }
      case (_, _) => ???

    // `Plan`'s `OpType` member IS the underlying `DoneOperation` — no need to re-walk `done`'s
    // `Operations` tuple by index to recover it. The final `.asInstanceOf[R]` below makes an
    // `OutputType`/`R` correspondence unnecessary here too.
    val operation: Expr[? <: DoneOperation { type OuterType = Real }] = Type.of[Plan] match
      case '[type op <: DoneOperation { type OuterType = Real }; OpPlan { type OpType = op }] =>
        '{ $done.operations($index).asInstanceOf[op] }

    val argsTuple = Expr.ofRefinedTuple(decodedArgs)

    val res = '{
      val op = $operation
      $done.invoke(op, $api, $argsTuple.asInstanceOf[op.Args])
    }
    '{ $res.asInstanceOf[R] }

  private def reject(inv: RawInvocation[?]): Nothing =
    throw new IllegalArgumentException("unknown rpc name: " + inv.rpcName)
