package mrpc.derive

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

/**
 * Server-adapter derivation: builds an `AsRaw[RawRpc[Raw], Real]` that turns a real trait instance
 * into a transport-facing [[RawRpc]]. The generated `RawRpc[Raw]` dispatches each incoming
 * [[RawInvocation]] by `rpcName`, decodes every argument to its EXACT declared parameter type via a
 * summoned `AsReal[Raw, paramType]`, calls the real method DIRECTLY (`api.<member>(args)`, commons-
 * style — no reflection mirror), and encodes the result back to `Raw`.
 *
 * Decoding to the exact declared parameter type before the call means the generated `api.method(...)`
 * type-checks against the member's own signature, so no boxing/unboxing mismatch can crash even on
 * primitives or wrapper types.
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

    '{
      new AsRaw[RawRpc[Raw], Real]:
        def asRaw(api: Real): RawRpc[Raw] =
          new RawRpc[Raw]:
            def fire(invocation: RawInvocation[Raw]): Unit =
              ${ fireBody[Raw, Real]('api, 'invocation, plans) }
            def call(invocation: RawInvocation[Raw]): Future[Raw] =
              ${ callBody[Raw, Real]('api, 'invocation, plans, ecExpr) }
            def get(invocation: RawInvocation[Raw]): RawRpc[Raw] =
              ${ getBody[Raw, Real]('api, 'invocation, plans) }
    }

  // --- arity-partitioned dispatch bodies ---

  private def fireBody[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
  )(using Quotes,
  ): Expr[Unit] =
    val arms = plans.filter(p =>
      p.arity match
        case Arity.Fire => true;
        case _ => false,
    )
    matchOnName[Raw, Unit](inv, arms, '{ () }) { plan =>
      val invokeTerm = invokeOp[Raw, Real](api, inv, plan)
      // Fire ops return Unit; invoking for its side effect is the whole job.
      '{ ${ invokeTerm.asExprOf[Any] }; () }
    }

  private def callBody[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plans: List[OpPlan],
    ec: Expr[ExecutionContext],
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
              val resultExpr = invokeOp[Raw, Real](api, inv, plan).asExprOf[Future[r]]
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
              // RECURSION SEAM (lazily recursive): the sub-RPC's server adapter is summoned at this
              // SINGLE site, then consumed through `AsRaw.makeLazy` so the recursive instance is forced
              // lazily (at first runtime `get`), NOT during macro expansion. A self- or mutually-
              // referential sub-RPC binds its adapter as a `lazy val given = makeLazy(...)`; the eager
              // summon here resolves to that already-declared lazy given instead of re-entering
              // `materialize*[Sub]`, so derivation reaches a fixed point with one real adapter per type.
              // Non-recursive sub-RPCs are unaffected. Keep it a SINGLE summon site.
              val subAdapter = Expr
                .summon[AsRaw[RawRpc[Raw], sub]]
                .getOrElse(
                  report.errorAndAbort(
                    s"unsupported result type / no sub-RPC conversion AsRaw[RawRpc[Raw], " +
                      s"${TypeRepr.of[sub].show}] for '${plan.rpcName}'",
                  ),
                )
              // Read the real sub-RPC instance off the api (a no-arg getter) and adapt it.
              val subInstance = invokeOp[Raw, Real](api, inv, plan).asExprOf[sub]
              '{ AsRaw.makeLazy[RawRpc[Raw], sub]($subAdapter).asRaw($subInstance) }
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
   * Decodes the invocation's flat arguments to the operation's exact declared parameter types and
   * emits a DIRECT call to the real member — `api.<member>(args...)` — re-nested into the member's own
   * parameter lists (commons-style; no reflection mirror, no runtime invoke). The returned term is
   * statically the member's result type; callers ascribe it for their arity. `inv.args` is nested per
   * parameter list (`List[List[Raw]]`) and flattened here before decoding.
   */
  private def invokeOp[Raw: Type, Real: Type](
    api: Expr[Real],
    inv: Expr[RawInvocation[Raw]],
    plan: OpPlan,
  )(using q: Quotes,
  ): q.reflect.Term =
    import q.reflect.*

    // Recover the exact trait member this plan describes — same enumeration + order as the matcher.
    val member = OpReflect.operationMembers[Real](plan.index)

    val flatArgs = '{ ${ inv }.args.flatten }

    // Decode each param to its EXACT declared type via a summoned AsReal[Raw, paramType]; the term is
    // then statically that type, so the generated `api.method(...)` type-checks against the signature.
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

    // Re-nest the flat decoded args into the member's own parameter lists, then call directly:
    //   no-parens `def f`      -> `api.f`        (sizes Nil)
    //   empty-parens `def f()` -> `api.f()`      (sizes List(0))
    //   `def f(a)(b, c)`       -> `api.f(a)(b,c)` (sizes List(1, 2))
    val sizes = OpReflect.paramListSizes(member)
    val argss = splitBySizes(decodedArgs, sizes)
    val sel = Select(api.asTerm, member) // select by Symbol (overload-safe), not by name
    if argss.isEmpty then sel else sel.appliedToArgss(argss)

  /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
  private def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
    sizes
      .foldLeft((items, List.empty[List[A]])) { case ((remaining, acc), n) =>
        val (group, rest) = remaining.splitAt(n)
        (rest, group :: acc)
      }
      ._2
      .reverse
