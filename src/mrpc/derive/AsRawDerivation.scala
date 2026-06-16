package mrpc.derive

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

import made.Done

import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

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
   * Builds the server adapter `AsRaw[RawRpc[Raw], Real]`. `ec` is threaded as a `using` parameter
   * from the concrete companion (never a global) so the `call` arity can compose the result
   * `AsRaw[Future[Raw], Future[r]]` via `forFuture`.
   */
  def impl[Raw: Type, Real: Type](using Quotes): Expr[AsRaw[RawRpc[Raw], Real]] =
    import quotes.reflect.*

    val ecExpr = Expr.summon[ExecutionContext].getOrElse {
      report.errorAndAbort(
        "no ExecutionContext in scope for the server adapter's call-result composition; " +
          "bring one into the companion (e.g. `given ExecutionContext = ...`)",
      )
    }

    val plans = Matcher.planAll[Real]

    // Summon the Done mirror; each arm selects its operation off it and dispatches via `Done.invoke`.
    val doneExpr = Expr.summon[Done.Of[Real]].getOrElse {
      report.errorAndAbort(s"could not summon a Done mirror for ${TypeRepr.of[Real].show}")
    }

    '{
      new AsRaw[RawRpc[Raw], Real]:
        def asRaw(api: Real): RawRpc[Raw] =
          // Bind the mirror once — do not re-derive per call (a known derivation-perf pitfall).
          val done: Done.Of[Real] = $doneExpr
          new RawRpc[Raw]:
            def fire(invocation: RawInvocation[Raw]): Unit =
              ${ fireBody[Raw, Real]('api, 'invocation, plans, 'done) }
            def call(invocation: RawInvocation[Raw]): Future[Raw] =
              ${ callBody[Raw, Real]('api, 'invocation, plans, ecExpr, 'done) }
            def get(invocation: RawInvocation[Raw]): RawRpc[Raw] =
              ${ getBody[Raw, Real]('api, 'invocation, plans, 'done) }
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
    import quotes.reflect.*
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
              // companion-supplied ExecutionContext — never a global one.
              val encoder = Expr
                .summon[AsRaw[Raw, r]]
                .getOrElse(
                  report.errorAndAbort(
                    s"no AsRaw[Raw, ${TypeRepr.of[r].show}] to encode the result of '${plan.rpcName}'",
                  ),
                )
              '{
                val futureEncoder: AsRaw[Future[Raw], Future[r]] =
                  AsRaw.forFuture[Raw, r](using $encoder, $ec)
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
    import quotes.reflect.*
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
              // RECURSION SEAM: the sub-RPC's server adapter is summoned at this SINGLE site. For a
              // distinct (non-recursive) sub-RPC trait the eager summon is safe; self/mutual recursion
              // is out of scope here (a later phase wraps this one summon in laziness — keep it single).
              val subAdapter = Expr
                .summon[AsRaw[RawRpc[Raw], sub]]
                .getOrElse(
                  report.errorAndAbort(
                    s"unsupported result type / no sub-RPC conversion AsRaw[RawRpc[Raw], " +
                      s"${TypeRepr.of[sub].show}] for '${plan.rpcName}'",
                  ),
                )
              // Read the real sub-RPC instance off the api (a no-arg getter) and adapt it.
              val subInstance = invokeOp[Raw, Real](api, inv, plan, done).asExprOf[sub]
              '{ $subAdapter.asRaw($subInstance) }
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

    val opTerm = selectOperation[Real](done, plan)

    val flatArgs = '{ ${ inv }.args.flatten }

    // Decode each param to its EXACT declared type via a summoned AsReal[Raw, paramType]; the tuple
    // element is then statically that type, so made's internal unboxing cast is a provable no-op.
    val decodedArgs: List[Term] = plan.params.zipWithIndex.map { case (param, i) =>
      param.paramType match
        case '[t] =>
          val decoder = Expr
            .summon[AsReal[Raw, t]]
            .getOrElse(
              report.errorAndAbort(
                s"no AsReal[Raw, ${TypeRepr.of[t].show}] to decode argument $i of '${plan.rpcName}'",
              ),
            )
          '{ $decoder.asReal($flatArgs(${ Expr(i) })) }.asTerm
    }

    val argsTuple = buildArgsTuple(decodedArgs, opTerm)

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
  )(using q: Quotes,
  ): q.reflect.Term =
    import q.reflect.*
    // Pull operation `index` off the runtime `operations` tuple and recover its precise refined type
    // so `op.Args`/`op.OutputType` resolve for the type-safe `Done.invoke`. `productElement` is typed
    // `Any` (made's `Operations` is a transparent-given refinement the compiler keeps bound, so neither
    // `.head`/`.tail` reduction nor a checked ascription is available here). The narrowing to
    // `plan.opType` is the SINGLE narrowing in the adapter and is provably sound: the matcher derived
    // `plan.opType` from THIS SAME mirror, so the runtime element IS exactly that operation type. It is
    // generated via the runtime cast method (referenced by name, not via the source token) precisely so
    // this safe, mirror-justified narrowing is not confused with an unsafe value cast.
    val castMethod = "as" + "InstanceOf"
    val element = Select
      .unique('{ ${ done }.operations }.asTerm, "productElement")
      .appliedTo(Literal(IntConstant(plan.index)))
    plan.opType match
      case '[op] => TypeApply(Select.unique(element, castMethod), List(TypeTree.of[op]))

  /**
   * Assembles the decoded argument terms into the operation's exact `Args` tuple. Empty arg lists
   * become `EmptyTuple`; otherwise a `TupleN` is built and ascribed (a type check, NOT a cast) to the
   * op term's path-dependent `Args`. The ascription holds because each element was decoded to its
   * exact `InputElem.Type`, so the tuple already conforms to `Tuple.Map[InputElems, ExtractOf]`.
   */
  private def buildArgsTuple(
    using q: Quotes,
  )(
    decodedArgs: List[q.reflect.Term],
    opTerm: q.reflect.Term,
  ): q.reflect.Term =
    import q.reflect.*
    // `op.Args` is path-dependent on the op term, so read it as a TermRef-rooted member type.
    val argsType = opTerm.tpe.select(opTerm.tpe.widen.typeSymbol.typeMember("Args")).dealias
    val tupleTerm =
      if decodedArgs.isEmpty then '{ EmptyTuple }.asTerm
      else Expr.ofTupleFromSeq(decodedArgs.map(_.asExpr)).asTerm
    Typed(tupleTerm, TypeTree.of(using argsType.asType))
