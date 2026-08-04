package mrpc.derive

import made.{Done, DoneOperation}
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
  inline def impl[Raw, Real: Done.Of](using ExecutionContext): AsRaw[RawRpc[Raw], Real] =
    (api: Real) => buildRawRpc[Raw, Real](api)

  /**
   * Macro entry: assembles the dispatching `RawRpc[Raw]` for a `Real` instance. `Done.Of[Real]` and
   * `Plans[Real]` are summoned ONCE here (via their own context bounds) and shared across the
   * `fire`/`call`/`get` bodies below — unlike three independent macro entries, which would each
   * re-derive them.
   */
  inline def buildRawRpc[Raw, Real: {Done.Of as done, Plans as plans}](api: Real)(using ec: ExecutionContext)
    : RawRpc[Raw] =
    ${ buildRawRpcImpl[Raw, Real, plans.Underlying]('api, 'done, 'ec) }

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
    // `zipWithIndex` HERE (not upfront in some shared pre-computed list) is what makes `index` mean
    // "position in `Done.Operations`": the filter below drops entries, so an index taken from the
    // FILTERED list would point at the wrong operation once `invokeOp` uses it to read
    // `done.operations(index)`.
    val arms = plans.zipWithIndex.filter((opType, _) =>
      opType match
        case '[{ type ArityInfo = ArityTag.Fire }] => true
        case _ => false,
    )
    matchOnName[Raw, Unit](inv, arms, '{ () }) { (opType, index) =>
      '{ ${ invokeOp[Raw, Real, Any](api, inv, opType, index, done) }: Unit }
    }

  private def callBody[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    ec: Expr[ExecutionContext],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Future[Raw]] =
    val plans = TupleTraverse.traverseTuple[Plans, OpPlan]
    val arms = plans.zipWithIndex.filter((opType, _) =>
      (opType.runtimeChecked match
        case '[OpPlan { type ArityInfo = a }] => Type.of[a]
      ) match
        case '[ArityTag.CallOf[?]] => true
        case _ => false,
    )
    matchOnName[Raw, Future[Raw]](inv, arms, reject(inv)) { (opType, index) =>
      opType match
        case '[{ type ArityInfo = ArityTag.CallOf[r] }] =>
          val resultExpr = invokeOp[Raw, Real, Future[r]](api, inv, opType, index, done)
          // Compose the leaf result encoder over Future via `forFuture`, threading the
          // companion-supplied ExecutionContext — never a global one. The encoder is resolved in
          // the generated code via `summonInline` (which reports a missing `AsRaw[Raw, r]` itself).
          '{
            val futureEncoder: AsRaw[Future[Raw], Future[r]] =
              AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], $ec)
            futureEncoder.asRaw($resultExpr)
          }
        case _ => reject(inv)
    }

  private def getBody[Raw: Type, Real: Type, Plans <: Tuple: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    val plans = TupleTraverse.traverseTuple[Plans, OpPlan]
    val arms = plans.zipWithIndex.filter((opType, _) =>
      opType match
        case '[OpPlan { type ArityInfo = a }] =>
          Type.of[a] match
            case '[ArityTag.GetOf[?]] => true
            case _ => false,
    )
    matchOnName[Raw, RawRpc[Raw]](inv, arms, reject(inv)) { (opType, index) =>
      opType match
        case '[{ type ArityInfo = ArityTag.GetOf[sub] }] =>
          val subInstance = invokeOp[Raw, Real, sub](api, inv, opType, index, done)
          '{
            AsRaw
              .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
              .asRaw($subInstance)
          }
        case _ => reject(inv)
    }

  // --- shared helpers ---

  /**
   * Builds `inv.rpcName match { case "<name1>" => <arm1> ; ... ; case _ => <reject> }` over the given
   * plans. Only the compile-time-known names produce arms; any other name hits `reject` (security:
   * unknown names are never silently accepted).
   */
  private def matchOnName[Raw: Type, Res: Type](
    inv: Expr[RawInvocation[Raw]],
    plans: List[(Type[? <: OpPlan], Int)],
    reject: Expr[Res],
  )(
    arm: (Type[? <: OpPlan], Int) => Expr[Res],
  )(using Quotes,
  ): Expr[Res] =
    import quotes.reflect.*
    val scrutinee = '{ ${ inv }.rpcName }.asTerm
    val caseDefs = plans.map { (opType, index) =>
      val rpcName = opType.runtimeChecked match
        case '[type n <: String; OpPlan { type RpcName = n }] =>
          Type.valueOfConstant[n].getOrElse(report.errorAndAbort("RpcName is not a string literal")).toString
      CaseDef(Literal(StringConstant(rpcName)), None, arm(opType, index).asTerm)
    }
    val default = CaseDef(Wildcard(), None, reject.asTerm)
    Match(scrutinee, caseDefs :+ default).asExprOf[Res]

  /** Each parameter's declared `ParamType`, in the op's declaration order, off `plan`'s `Params`. */
  private def paramTypesOf(plan: Type[?])(using Quotes): List[Type[?]] =
    plan.runtimeChecked match
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
  private def invokeOp[Raw: Type, Real: Type, R: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    opType: Type[? <: OpPlan],
    index: Int,
    done: Expr[Done.Of[Real]],
  )(using quotes: Quotes,
  ): Expr[R] =
    val flatArgs = '{ ${ inv }.args.flatten }

    val decodedArgs: List[Expr[?]] = paramTypesOf(opType).zipWithIndex.map:
      case ('[t], i) =>
        '{ scala.compiletime.summonInline[AsReal[Raw, t]].asReal($flatArgs(${ Expr(i) })) }
      case (_, _) => ???

    // `opType`'s `OpType` member IS the underlying `DoneOperation` — no need to re-walk `done`'s
    // `Operations` tuple by index to recover it. The final `.asInstanceOf[R]` below makes an
    // `OutputType`/`R` correspondence unnecessary here too.
    val operation: Expr[? <: DoneOperation { type OuterType = Real }] = opType match
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
