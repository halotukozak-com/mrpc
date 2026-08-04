package mrpc
package derive

import made.{containsOnly, Done, DoneOperation}
import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

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
    ${ buildRawRpcImpl[Raw, Real, Plans]('api, 'done, 'ec) }

  /**
   * `mkRawRpc` is an ordinary, non-`inline` method — an anonymous class defined directly inside an
   * `inline`/macro body is otherwise recompiled at every inline site.
   */
  private def mkRawRpc[Raw](
    fireFn: RawInvocation[Raw] => Unit,
    callFn: RawInvocation[Raw] => Future[Raw],
    getFn: RawInvocation[Raw] => RawRpc[Raw],
  ): RawRpc[Raw] =
    new RawRpc[Raw]:
      def fire(invocation: RawInvocation[Raw]): Unit = fireFn(invocation)
      def call(invocation: RawInvocation[Raw]): Future[Raw] = callFn(invocation)
      def get(invocation: RawInvocation[Raw]): RawRpc[Raw] = getFn(invocation)

  /**
   * The `Names` shared across all three bodies below: every op's resolved `RpcName`, in `Plans`
   * order — computed ONCE here so `fire`/`call`/`get` all dispatch off the exact same tuple of
   * literal name types, rather than each re-deriving (and potentially mis-filtering) its own.
   */
  private def buildRawRpcImpl[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    done: Expr[Done.Of[Real]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    type Names = Tuple.Map[Plans, [op] =>> op match
      case ([n] =>> OpPlan { type RpcName = n })[name] => name]
    '{
      mkRawRpc[Raw](
        (invocation: RawInvocation[Raw]) => ${ fireBody[Raw, Real, Plans, Names](api, 'invocation, done) },
        (invocation: RawInvocation[Raw]) => ${ callBody[Raw, Real, Plans, Names](api, 'invocation, ec, done) },
        (invocation: RawInvocation[Raw]) => ${ getBody[Raw, Real, Plans, Names](api, 'invocation, done) },
      )
    }

  // --- arity-partitioned dispatch bodies ---

  /**
   * Each body builds `values` over EVERY entry of `plans` (not just its own arity), positionally
   * aligned with the shared `Names` by construction — an op outside this body's own arity gets a
   * `reject` thunk in its slot instead of real dispatch logic. This is what keeps `Names`/`values`
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
  private def fireBody[Raw: Type, Real: Type, Plans <: Tuple: Type, Names <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Unit] =
    // Walks `Plans` directly via quote-pattern recursion — `h` is bound fresh at each step (with its
    // OWN precise refined type, `Type[h]` given automatically by the pattern) rather than first
    // collected into a `List[Type[? <: OpPlan]]` and re-matched afterward, so it never widens to an
    // unnamed wildcard and never needs an `op.Underlying`-style workaround to recover it.
    def arms[Tup <: Tuple: Type](index: Int): List[Expr[() => Unit]] = Type.of[Tup] match
      case '[EmptyTuple] => Nil
      case '[type h <: OpPlan; h *: t] =>
        val arm = Type.of[h] match
          case '[OpPlan { type ArityInfo = ArityTag.Fire }] =>
            '{ () => ${ invokeOp[Raw, Real, Any, h](api, inv, index, done) }: Unit }
          case _ => '{ () => ${ reject(inv) } }
        arm :: arms[t](index + 1)

    val values = Expr.ofRefinedTuple(arms[Plans](0))

    values match
      case '{ type values <: Tuple; $values: values } =>
        '{
          ${
            matchFromImpl[Names, values](
              '{ ${ inv }.rpcName },
              '{ NamedTuple.build[Names]()($values) },
              reject(inv),
            ).asExprOf[() => Unit]
          }()
        }

  private def callBody[Raw: Type, Real: Type, Plans <: Tuple: Type, Names <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    ec: Expr[ExecutionContext],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Future[Raw]] =
    def arms[Tup <: Tuple: Type](index: Int): List[Expr[() => Future[Raw]]] = Type.of[Tup] match
      case '[EmptyTuple] => Nil
      case '[type h <: OpPlan; h *: t] =>
        val arm = Type.of[h] match
          case '[OpPlan { type ArityInfo = ArityTag.CallOf[r] }] =>
            '{ () =>
              val futureEncoder = AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], $ec)
              futureEncoder.asRaw(${ invokeOp[Raw, Real, Future[r], h](api, inv, index, done) })
            }
          case _ => '{ () => ${ reject(inv) } }
        arm :: arms[t](index + 1)

    val values = Expr.ofRefinedTuple(arms[Plans](0))

    values match
      case '{ type values <: Tuple; $values: values } =>
        '{
          ${
            matchFromImpl[Names, values](
              '{ ${ inv }.rpcName },
              '{ NamedTuple.build[Names]()($values) },
              reject(inv),
            ).asExprOf[() => Future[Raw]]
          }()
        }

  private def getBody[Raw: Type, Real: Type, Plans <: Tuple: Type, Names <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    def arms[Tup <: Tuple: Type](index: Int): List[Expr[() => RawRpc[Raw]]] = Type.of[Tup] match
      case '[EmptyTuple] => Nil
      case '[type h <: OpPlan; h *: t] =>
        val arm = Type.of[h] match
          case '[OpPlan { type ArityInfo = ArityTag.GetOf[sub] }] =>
            '{ () =>
              AsRaw
                .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
                .asRaw(${ invokeOp[Raw, Real, sub, h](api, inv, index, done) })
            }
          case _ => '{ () => ${ reject(inv) } }
        arm :: arms[t](index + 1)

    val values = Expr.ofRefinedTuple(arms[Plans](0))

    values match
      case '{ type values <: Tuple; $values: values } =>
        '{
          ${
            matchFromImpl[Names, values](
              '{ ${ inv }.rpcName },
              '{ NamedTuple.build[Names]()($values) },
              reject(inv),
            ).asExprOf[() => RawRpc[Raw]]
          }()
        }

  // --- shared helpers ---

  /** Each parameter's declared `ParamType`, in the op's declaration order, off `plan`'s `Params`. */
  private def paramTypesOf[Plan <: OpPlan: Type](using Quotes): List[Type[?]] =
    Type.of[Plan] match
      case '[type ps <: Tuple; OpPlan { type Params = ps }] =>
        TupleTraverse.traverseTuple[ps, ParamPlan].map { case '[ParamPlan { type ParamType = t }] => Type.of[t] }

  /**
   * Decodes the invocation's flat arguments to the operation's exact `InputElem.Type`s, assembles
   * `op.Args`, and emits `Done.invoke(done, op, api, args)`. The returned term is statically typed as
   * the operation's `OutputType`; callers ascribe it for their arity.
   *
   * `inv.args` is nested per parameter list (`List[List[Raw]]`); it is flattened in `InputElems`
   * order before decoding, matching made's flattened `Args` contract.
   */
  private def invokeOp[Raw: Type, Real: Type, R: Type, Plan <: OpPlan: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    index: Int,
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
        '{ $done.operations(${ Expr(index) }).asInstanceOf[op] }

    val argsTuple = Expr.ofRefinedTuple(decodedArgs)

    val res = '{
      val op = $operation
      $done.invoke(op, $api, $argsTuple.asInstanceOf[op.Args])
    }
    '{ $res.asInstanceOf[R] }

  private def reject(inv: Expr[RawInvocation[?]])(using Quotes) = '{
    throw new IllegalArgumentException("unknown rpc name: " + $inv.rpcName)
  }
