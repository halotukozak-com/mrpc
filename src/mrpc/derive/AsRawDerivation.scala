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

  private def buildRawRpcImpl[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    done: Expr[Done.Of[Real]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    '{
      mkRawRpc[Raw](
        (invocation: RawInvocation[Raw]) => ${ fireBody[Raw, Real, Plans](api, 'invocation, done) },
        (invocation: RawInvocation[Raw]) => ${ callBody[Raw, Real, Plans](api, 'invocation, ec, done) },
        (invocation: RawInvocation[Raw]) => ${ getBody[Raw, Real, Plans](api, 'invocation, done) },
      )
    }

  // --- arity-partitioned dispatch bodies ---

  private def fireBody[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Unit] =
    val plans = TupleTraverse.traverseTuple[Plans, OpPlan]

    val fireArms = plans.zipWithIndex.collect { case (op @ '[{ type ArityInfo = ArityTag.Fire }], index) =>
      (op, index)
    }

    if fireArms.isEmpty then '{ () }
    else
      val names = TupleTraverse.foldTuple(fireArms.map((op, _) => op).collect {
        case '[type n; OpPlan { type RpcName = n }] => Type.of[n]
      })

      val values =
        Expr.ofRefinedTuple(fireArms.map { (op, index) =>
          given Type[op.Underlying] = op
          val underlying = invokeOp[Raw, Real, Any, op.Underlying](api, inv, index, done)
          '{ () => $underlying: Unit }
        })

      (names, values) match
        case ('[type names <: Tuple; names], '{ type values <: Tuple; $values: values }) =>
          '{
            ${
              matchFromImpl[names, values](
                '{ ${ inv }.rpcName },
                '{ NamedTuple.build[names]()($values) },
                '{ () => () }.asExprOf[Tuple.Union[values]],
              ).asExprOf[() => Unit]
            }()
          }
        case (_, _) => ???

  private def callBody[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    ec: Expr[ExecutionContext],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Future[Raw]] =
    val plans = TupleTraverse.traverseTuple[Plans, OpPlan]

    val callArms = plans.zipWithIndex.collect {
      case (op @ '[OpPlan { type ArityInfo = ArityTag.CallOf[?] }], index) => (op, index)
    }

    val names = TupleTraverse.foldTuple(callArms.map((op, _) => op).collect {
      case '[type n; OpPlan { type RpcName = n }] => Type.of[n]
    })

    val values =
      Expr.ofRefinedTuple(callArms.map { (op, index) =>
        given Type[op.Underlying] = op
        Type.of[op.Underlying] match
          case '[OpPlan { type ArityInfo = ArityTag.CallOf[r] }] =>
            val underlying = invokeOp[Raw, Real, Future[r], op.Underlying](api, inv, index, done)
            '{ () =>
              val futureEncoder = AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], $ec)
              futureEncoder.asRaw($underlying)
            }
          case _ => '{ () => ${ reject(inv) } }
      })

    (names, values) match
      case ('[type names <: Tuple; names], '{ type values <: Tuple; $values: values }) =>
        '{
          ${
            matchFromImpl[names, values](
              '{ ${ inv }.rpcName },
              '{ NamedTuple.build[names]()($values) },
              reject(inv),
            ).asExprOf[() => Future[Raw]]
          }()
        }
      case (_, _) => ???

  private def getBody[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    val plans = TupleTraverse.traverseTuple[Plans, OpPlan]

    val getArms = plans.zipWithIndex.collect { case (op @ '[{ type ArityInfo = ArityTag.GetOf[sub] }], index) =>
      (op, index)
    }

    val names = TupleTraverse.foldTuple(getArms.map((op, _) => op).collect {
      case '[type n; OpPlan { type RpcName = n }] => Type.of[n]
    })

    val values =
      Expr.ofRefinedTuple(getArms.collect { case (op @ '[{ type ArityInfo = ArityTag.GetOf[sub] }], index) =>
        given Type[op.Underlying] = op
        val underlying = invokeOp[Raw, Real, sub, op.Underlying](api, inv, index, done)
        '{ () =>
          AsRaw.makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]]).asRaw($underlying)
        }
      })

    (names, values) match
      case ('[type names <: Tuple; names], '{ type values <: Tuple; $values: values }) =>
        '{
          ${
            matchFromImpl[names, values](
              '{ ${ inv }.rpcName },
              '{ NamedTuple.build[names]()($values) },
              reject(inv),
            ).asExprOf[() => RawRpc[Raw]]
          }()
        }
      case (_, _) => ???

  // --- shared helpers ---

  /** Each parameter's declared `ParamType`, in the op's declaration order, off `plan`'s `Params`. */
  private def paramTypesOf[Op <: OpPlan: Type](using Quotes): List[Type[?]] =
    Type.of[Op] match
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
  private def invokeOp[Raw: Type, Real: Type, R: Type, Op <: OpPlan: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    index: Int,
    done: Expr[Done.Of[Real]],
  )(using quotes: Quotes,
  ): Expr[R] =
    val flatArgs = '{ ${ inv }.args.flatten }

    val decodedArgs: List[Expr[?]] = paramTypesOf[Op].zipWithIndex.map:
      case ('[t], i) =>
        '{ scala.compiletime.summonInline[AsReal[Raw, t]].asReal($flatArgs(${ Expr(i) })) }
      case (_, _) => ???

    // `opType`'s `OpType` member IS the underlying `DoneOperation` — no need to re-walk `done`'s
    // `Operations` tuple by index to recover it. The final `.asInstanceOf[R]` below makes an
    // `OutputType`/`R` correspondence unnecessary here too.
    val operation: Expr[? <: DoneOperation { type OuterType = Real }] = Type.of[Op] match
      case '[type op <: DoneOperation { type OuterType = Real }; OpPlan { type OpType = op }] =>
        '{ $done.operations(${ Expr(index) }).asInstanceOf[op] }

    val argsTuple = Expr.ofRefinedTuple(decodedArgs)

    val res = '{
      val op = $operation
      $done.invoke(op, $api, $argsTuple.asInstanceOf[op.Args])
    }
    '{ $res.asInstanceOf[R] }

  private def reject(inv: Expr[RawInvocation[?]])(using Quotes) = '{
    throw new IllegalArgumentException("unknown rpc name for get: " + $inv.rpcName)
  }
