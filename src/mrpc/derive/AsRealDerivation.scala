package mrpc.derive

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

/**
 * Client-proxy derivation: builds an `AsReal[RawRpc[Raw], Real]` that turns a transport-facing
 * [[RawRpc]] into a concrete implementation of the real trait. Each generated method packages a
 * [[RawInvocation]] (the matcher's resolved `rpcName` + per-param-list encoded arguments) and
 * forwards to the underlying `RawRpc[Raw]`'s `fire`/`call`/`get` by arity, decoding the result back
 * to the method's EXACT declared type via a summoned `AsReal`.
 *
 * scala3#25245 discipline (the `VerifyError` on macro-generated trait impls that made works around
 * with `DoneOperationWorkaround[Outer]`): the proxy is generated against the CONCRETE `Real` type
 * argument with no abstract type members. Method bodies and the argument lists are built with the
 * exact concrete parameter/result types read off the trait's own method symbols, so `-Ycheck:macros`
 * stays clean even on primitive/wrapper-returning methods (e.g. `echoBool: Future[Boolean]`).
 *
 * The arity routing mirrors the server adapter inversely: `Unit` -> `fire`, `Future[X]` -> `call`
 * (result decoded via `forFuture`), sub-RPC -> `get` (a SINGLE summon seam, lazily recursive via
 * `AsReal.makeLazy` so self/mutually-referential sub-RPCs derive without infinite inline expansion).
 */
object AsRealDerivation:

  /**
   * Builds the client proxy `AsReal[RawRpc[Raw], Real]`. `ec` is threaded as a `using` parameter from
   * the concrete companion (never a global) so the `call` arity can compose the result decoder
   * `AsReal[Future[Raw], Future[r]]` via `forFuture`.
   */
  def impl[Raw: Type, Real: Type](using Quotes): Expr[AsReal[RawRpc[Raw], Real]] =
    import quotes.reflect.*

    val ecExpr = Expr.summon[ExecutionContext].getOrElse {
      report.errorAndAbort(
        "no ExecutionContext in scope for the client proxy's call-result composition; " +
          "bring one into the companion (e.g. `given ExecutionContext = ...`)",
      )
    }

    // The matcher's plans, in `Done.Operations` (declaration) order. The proxy aligns each generated
    // method to its plan BY INDEX rather than by label, because overloaded methods (e.g. two
    // `lookup`s) share a label but have distinct signatures and distinct plans — a label map would
    // collapse them.
    val plans: List[OpPlan] = Matcher.planAll[Real]

    '{
      new AsReal[RawRpc[Raw], Real]:
        def asReal(raw: RawRpc[Raw]): Real =
          ${ proxyExpr[Raw, Real]('raw, plans, ecExpr) }
    }

  /**
   * Generates the concrete `new Real { <one body per abstract method> }` proxy. The class and its
   * method symbols are synthesized with [[quotes.reflect.Symbol.newClass]] so each override matches
   * the trait's declared method signature exactly (concrete parameter and result types), which is the
   * #25245-safe construction.
   */
  private def proxyExpr[Raw: Type, Real: Type](
    raw: Expr[RawRpc[Raw]],
    plans: List[OpPlan],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[Real] =
    import quotes.reflect.*

    val parents = List(TypeTree.of[Object], TypeTree.of[Real])

    // Recover each member symbol straight from its planned operation type (`OuterType` = Real), so the
    // member-to-plan correspondence is exact even across overloads. `memberType` then yields the
    // member's precise (parameter + result) signature for the synthesized override.
    val membersToImplement: List[Symbol] = plans.map(planMember[Real])

    def decls(cls: Symbol): List[Symbol] =
      membersToImplement.map { m =>
        Symbol.newMethod(
          cls,
          m.name,
          TypeRepr.of[Real].memberType(m),
          Flags.Override,
          Symbol.noSymbol,
        )
      }

    val proxyName = Symbol.freshName("RawClientProxy")
    val clsSymbol = Symbol.newClass(
      Symbol.spliceOwner,
      proxyName,
      parents.map(_.tpe),
      decls,
      selfType = None,
    )

    // `declaredMethods` preserves the order `decls` produced, so it aligns with `plans` by index.
    val methodDefs: List[DefDef] = clsSymbol.declaredMethods.zip(plans).map { case (methodSym, plan) =>
      DefDef(
        methodSym,
        // Re-own the body's locals (e.g. the `forFuture` decoder val) to the synthesized method, or
        // the backend rejects vals owned by the splice context.
        argss => Some(methodBody[Raw](argss, plan, raw, ec).changeOwner(methodSym)),
      )
    }

    val clsDef = ClassDef(clsSymbol, parents, methodDefs)
    val instance = Typed(
      Apply(Select(New(TypeIdent(clsSymbol)), clsSymbol.primaryConstructor), Nil),
      TypeTree.of[Real],
    )
    Block(List(clsDef), instance).asExprOf[Real]

  /**
   * Resolves the real-trait member symbol an [[OpPlan]] describes. The plan's `label` gives the
   * member name; for overloaded members (same name, distinct signatures) it disambiguates by matching
   * the member's flattened parameter types against the plan's `params` types.
   */
  private def planMember[Real: Type](plan: OpPlan)(using Quotes): quotes.reflect.Symbol =
    import quotes.reflect.*
    val realTpe = TypeRepr.of[Real]
    val candidates = realTpe.typeSymbol.methodMembers
      .filter(m => m.isDefDef && m.flags.is(Flags.Deferred) && m.name == plan.label)
    candidates match
      case single :: Nil => single
      case Nil =>
        report.errorAndAbort(s"no abstract member named '${plan.label}' on ${realTpe.show}")
      case many =>
        val wantParams = plan.params.map(p => TypeRepr.of(using p.paramType))
        many
          .find { m =>
            val mParams = flattenParamTypes(realTpe.memberType(m).widen)
            mParams.length == wantParams.length && mParams.zip(wantParams).forall { case (a, b) => a =:= b }
          }
          .getOrElse(
            report.errorAndAbort(
              s"could not disambiguate overloaded member '${plan.label}' by parameter types",
            ),
          )

  /** Flattens a (possibly curried) method type's parameter types in declaration order. */
  private def flattenParamTypes(
    using q: Quotes,
  )(
    mt: q.reflect.TypeRepr,
  ): List[q.reflect.TypeRepr] =
    import q.reflect.*
    mt match
      case MethodType(_, paramTypes, res) => paramTypes ++ flattenParamTypes(res)
      case _ => Nil

  /**
   * Builds one proxy method body: package a [[RawInvocation]] (resolved `rpcName` + nested encoded
   * args) and forward to `raw.fire`/`raw.call`/`raw.get` by the planned arity, decoding the result
   * back to the method's exact declared type.
   */
  private def methodBody[Raw: Type](
    using q: Quotes,
  )(
    argss: List[List[q.reflect.Tree]],
    plan: OpPlan,
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
  ): q.reflect.Term =
    import q.reflect.*

    // Positional argument terms, flattened across parameter lists in declaration order — the same
    // order the matcher's `plan.params` follow.
    val flatArgTerms: List[Term] = argss.flatten.collect { case t: Term => t }

    val invocation = invocationExpr[Raw](plan, flatArgTerms)

    plan.arity match
      case Arity.Fire =>
        '{ $raw.fire($invocation) }.asTerm
      case Arity.Call(resultType) =>
        resultType match
          case '[r] =>
            // Decode the raw `Future[Raw]` result back to the exact `Future[r]` via `forFuture`,
            // threading the companion-supplied ExecutionContext — never a global one.
            val decoder = Expr
              .summon[AsReal[Raw, r]]
              .getOrElse(
                report.errorAndAbort(
                  s"no AsReal[Raw, ${TypeRepr.of[r].show}] to decode the result of '${plan.rpcName}'",
                ),
              )
            '{
              val futureDecoder: AsReal[Future[Raw], Future[r]] =
                AsReal.forFuture[Raw, r](using $decoder, $ec)
              futureDecoder.asReal($raw.call($invocation))
            }.asTerm
      case Arity.Get(subRpcType) =>
        subRpcType match
          case '[sub] =>
            // RECURSION SEAM (lazily recursive): the sub-RPC's client proxy is summoned at this SINGLE
            // site, then consumed through `AsReal.makeLazy` so the recursive instance is forced lazily
            // (at first runtime `get`), NOT during macro expansion. A self- or mutually-referential
            // sub-RPC binds its proxy as a `lazy val given = makeLazy(...)`; the eager summon here
            // resolves to that already-declared lazy given instead of re-entering `materialize*[Sub]`,
            // so derivation reaches a fixed point with one real proxy per type. Non-recursive sub-RPCs
            // are unaffected. Keep it a SINGLE summon site.
            val subProxy = Expr
              .summon[AsReal[RawRpc[Raw], sub]]
              .getOrElse(
                report.errorAndAbort(
                  s"unsupported result type / no sub-RPC conversion AsReal[RawRpc[Raw], " +
                    s"${TypeRepr.of[sub].show}] for '${plan.rpcName}'",
                ),
              )
            '{ AsReal.makeLazy[RawRpc[Raw], sub]($subProxy).asReal($raw.get($invocation)) }.asTerm

  /**
   * Builds `RawInvocation(<rpcName>, <nested encoded args>)`. Each argument is encoded to `Raw` via a
   * summoned `AsRaw[Raw, paramType]`, then the flat encoded list is split back into `List[List[Raw]]`
   * per the op's `ParamLists` sizes — the exact inverse of the server adapter's `args.flatten`.
   */
  private def invocationExpr[Raw: Type](
    using q: Quotes,
  )(
    plan: OpPlan,
    flatArgTerms: List[q.reflect.Term],
  ): Expr[RawInvocation[Raw]] =
    import q.reflect.*

    // Encode each argument to its exact param type. `plan.params` aligns positionally with the
    // flattened argument terms (both follow declaration order).
    val encodedArgs: List[Expr[Raw]] = plan.params.zip(flatArgTerms).zipWithIndex.map { case ((param, argTerm), i) =>
      param.paramType match
        case '[t] =>
          val encoder = Expr
            .summon[AsRaw[Raw, t]]
            .getOrElse(
              report.errorAndAbort(
                s"no AsRaw[Raw, ${TypeRepr.of[t].show}] to encode argument $i of '${plan.rpcName}'",
              ),
            )
          // Apply `encoder.asRaw(argTerm)` via reflection rather than `argTerm.asExprOf[t]`: the
          // synthesized method parameter's term type is its own path, which the strict quote cast
          // rejects, while `Apply` type-checks the conformance the same way the source would.
          Select
            .unique(encoder.asTerm, "asRaw")
            .appliedTo(argTerm)
            .asExprOf[Raw]
    }

    // Split the flat encoded args into nested per-param-list lists. An empty-parens op (e.g. `ping()`,
    // `count()`) has `ParamLists = List(0)`, so it yields `List(List())`; commons/mrpc dispatch treats
    // such a call as `Nil` args, so collapse all-empty nestings to an empty outer list.
    val sizes = OpReflect.paramListSizes(plan.opType)
    val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
    val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
    val argsExpr: Expr[List[List[Raw]]] =
      if nested.forall(_.isEmpty) then '{ Nil } else Expr.ofList(nestedExprs)

    '{ RawInvocation[Raw](${ Expr(plan.rpcName) }, $argsExpr) }

  /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
  private def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
    sizes
      .foldLeft((items, List.empty[List[A]])) { case ((remaining, acc), n) =>
        val (group, rest) = remaining.splitAt(n)
        (rest, group :: acc)
      }
      ._2
      .reverse
