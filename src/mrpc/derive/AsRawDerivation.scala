package mrpc.derive

import made.Done
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
   * by the [[buildRawRpc]] macro. The `AsRaw` wrapper is ordinary Scala (a SAM lambda); only the
   * `RawRpc` itself — its arity-partitioned `fire`/`call`/`get` dispatch — is generated.
   */
  inline def impl[Raw, Real](using Done.Of[Real], ExecutionContext): AsRaw[RawRpc[Raw], Real] =
    (api: Real) => buildRawRpc[Raw, Real](api)

  /**
   * Macro entry: build the dispatching `RawRpc[Raw]` for a `Real` instance. The mirror and
   * `ExecutionContext` are summoned here and handed to [[buildRawRpcImpl]]. `ExecutionContext` is in
   * scope so the `call` arity can compose `AsRaw[Future[Raw], Future[r]]` via `forFuture`.
   */
  inline def buildRawRpc[Raw, Real](api: Real)(using Done.Of[Real], ExecutionContext): RawRpc[Raw] =
    ${ buildRawRpcImpl[Raw, Real]('api, 'summon, 'summon) }

  /**
   * Builds the `RawRpc[Raw]` whose `fire`/`call`/`get` dispatch incoming invocations by `rpcName`. The
   * anonymous class lives in this generated quote (not an `inline` body), and `plans` is computed ONCE
   * here and shared across the three arity-partitioned bodies.
   */
  private def buildRawRpcImpl[Raw: Type, Real: Type](
    api: Expr[Real],
    done: Expr[Done.Of[Real]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    val plans = Matcher.planAll[Real](done)
    '{
      new RawRpc[Raw]:
        def fire(invocation: RawInvocation[Raw]): Unit =
          ${ fireBody[Raw, Real](api, 'invocation, plans, done) }
        def call(invocation: RawInvocation[Raw]): Future[Raw] =
          ${ callBody[Raw, Real](api, 'invocation, plans, ec, done) }
        def get(invocation: RawInvocation[Raw]): RawRpc[Raw] =
          ${ getBody[Raw, Real](api, 'invocation, plans, done) }
    }

  // --- arity-partitioned dispatch bodies ---

  private def fireBody[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Unit] =
    val arms = plans.filter(p =>
      p.arity match
        case Arity.Fire => true;
        case _ => false,
    )
    matchOnName[Raw, Unit](inv, arms, '{ () }) { plan =>
      val invokeTerm = invokeOp[Raw, Real](api, inv, plan, done)
      // Fire ops return Unit; invoking for its side effect is the whole job.
      '{ ${ invokeTerm.asExprOf[Any] }; () }
    }

  private def callBody[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
    ec: Expr[ExecutionContext],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Future[Raw]] =
    val reject = '{
      throw new IllegalArgumentException(
        "unknown rpc name for call: " + ${ inv }.rpcName,
      )
    }
    val arms = plans.filter(p =>
      p.arity match
        case Arity.Call(_) => true;
        case _ => false,
    )
    matchOnName[Raw, Future[Raw]](inv, arms, reject) { plan =>
      plan.arity match
        case Arity.Call(resultType) =>
          resultType match
            case '[r] =>
              val resultExpr = invokeOp[Raw, Real](api, inv, plan, done).asExprOf[Future[r]]
              // Compose the leaf result encoder over Future via `forFuture`, threading the
              // companion-supplied ExecutionContext — never a global one. The encoder is resolved in
              // the generated code via `summonInline` (which reports a missing `AsRaw[Raw, r]` itself).
              '{
                val futureEncoder: AsRaw[Future[Raw], Future[r]] =
                  AsRaw.forFuture[Raw, r](using scala.compiletime.summonInline[AsRaw[Raw, r]], $ec)
                futureEncoder.asRaw($resultExpr)
              }
        case _ => reject
    }

  private def getBody[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[RawRpc[Raw]] =
    val reject = '{
      throw new IllegalArgumentException(
        "unknown rpc name for get: " + ${ inv }.rpcName,
      )
    }
    val arms = plans.filter(p =>
      p.arity match
        case Arity.Get(_) => true;
        case _ => false,
    )
    matchOnName[Raw, RawRpc[Raw]](inv, arms, reject) { plan =>
      plan.arity match
        case Arity.Get(subRpcType) =>
          subRpcType match
            case '[sub] =>
              val subInstance = invokeOp[Raw, Real](api, inv, plan, done).asExprOf[sub]
              '{
                AsRaw
                  .makeLazy[RawRpc[Raw], sub](compiletime.summonInline[AsRaw[RawRpc[Raw], sub]])
                  .asRaw($subInstance)
              }
        case _ => reject
    }

  // --- shared helpers ---

  /**
   * Builds `inv.rpcName match { case "<name1>" => <arm1> ; ... ; case _ => <reject> }` over the given
   * plans. Only the compile-time-known names produce arms; any other name hits `reject` (security:
   * unknown names are never silently accepted).
   */
  private def matchOnName[Raw: Type, Res: Type](
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
    reject: Expr[Res],
  )(
    arm: OpPlan => Expr[Res],
  )(using Quotes,
  ): Expr[Res] =
    import quotes.reflect.*
    val scrutinee = '{ ${ inv }.rpcName }.asTerm
    val caseDefs = plans.map { plan =>
      CaseDef(Literal(StringConstant(plan.rpcName)), None, arm(plan).asTerm)
    }
    val default = CaseDef(Wildcard(), None, reject.asTerm)
    Match(scrutinee, caseDefs :+ default).asExprOf[Res]

  /**
   * Decodes the invocation's flat arguments to the operation's exact `InputElem.Type`s, assembles
   * `op.Args`, and emits `Done.invoke(done, op, api, args)`. The returned term is statically typed as
   * the operation's `OutputType`; callers ascribe it for their arity.
   *
   * `inv.args` is nested per parameter list (`List[List[Raw]]`); it is flattened in `InputElems`
   * order before decoding, matching made's flattened `Args` contract.
   */
  private def invokeOp[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plan: OpPlan,
    done: Expr[Done.Of[Real]],
  )(using q: Quotes,
  ): q.reflect.Term =
    import q.reflect.*

    val opTerm = selectOperation[Real](done, plan).asTerm

    val flatArgs = '{ ${ inv }.args.flatten }

    // Decode each param to its EXACT declared type via a summoned AsReal[Raw, paramType]; the tuple
    // element is then statically that type, so made's internal unboxing cast is a provable no-op.
    val decodedArgs: List[Term] = plan.params.zipWithIndex.map { case (param, i) =>
      param.paramType match
        case '[t] =>
          '{ scala.compiletime.summonInline[AsReal[Raw, t]].asReal($flatArgs(${ Expr(i) })) }.asTerm
    }

    // The decoded args are each statically their exact `InputElem.Type`, so the tuple already conforms
    // to the op's `Args` (= `Tuple.Map[InputElems, ExtractOf]`) at the `Done.invoke` call below — no
    // explicit ascription to the path-dependent `op.Args` is needed.
    val argsTuple = Expr.ofTupleFromSeq(decodedArgs.map(_.asExpr)).asTerm

    // Done.invoke(done, op, api, args) — the type-safe extension (`Done.invoke[Real](done)(op, api,
    // args)`). op carries OuterType = Real and Args = the exact tuple we built, both checked against
    // `opTerm`'s refined type here. `invoke` is a top-level extension method on the `Done` companion.
    val invokeRef = TypeApply(
      Select.unique(Ref(TypeRepr.of[Done.type].termSymbol), "invoke"),
      List(TypeTree.of[Real]),
    )
    Apply(
      Apply(invokeRef, List(done.asTerm)),
      List(opTerm, api.asTerm, argsTuple),
    )

  /**
   * Selects `done.operations` element `index` and recovers its precise refined operation type so that
   * `op.Args`/`op.OutputType`/`op.OuterType` resolve for the type-safe `Done.invoke` call. See the
   * inline note on the single, mirror-justified narrowing this performs.
   */
  private def selectOperation[Real: Type](
    done: Expr[Done.Of[Real]],
    plan: OpPlan,
  )(using Quotes,
  ): Expr[Any] =
    // Pull operation `index` off the runtime `operations` tuple and narrow it to its precise refined
    // type so `op.Args`/`op.OutputType` resolve for the type-safe `Done.invoke`. `productElement` is
    // typed `Any` (made's `Operations` is a transparent-given refinement the compiler keeps bound, so
    // neither `.head`/`.tail` reduction nor a checked ascription is available). The narrowing to
    // `plan.opType` is the SINGLE narrowing in the adapter and is provably sound: the matcher derived
    // `plan.opType` from THIS SAME mirror, so the runtime element IS exactly that operation type.
    plan.opType match
      case '[op] => '{ $done.operations.productElement(${ Expr(plan.index) }).asInstanceOf[op] }

