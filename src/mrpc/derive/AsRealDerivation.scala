package mrpc.derive

import made.*
import mrpc.conv.{AsRaw, AsReal}
import mrpc.raw.{RawInvocation, RawRpc}

import scala.concurrent.{ExecutionContext, Future}
import scala.quoted.*

/**
 * Client-proxy derivation: builds an `AsReal[RawRpc[Raw], Real]` that turns a transport-facing
 * [[RawRpc]] into a concrete implementation of the real trait.
 *
 * The trait synthesis is delegated to made's `Done.materialize` (the tuple-of-handlers `.to[Real]`):
 * mrpc builds one handler per operation — each an `op.Args => op.OutputType` function that packages a
 * [[RawInvocation]] (the resolved `rpcName` + per-param-list encoded arguments) and forwards to the
 * underlying `RawRpc[Raw]`'s `fire`/`call`/`get` by arity, decoding the result back to the method's
 * exact declared type via a summoned `AsReal`. made wires method i to handler i (declaration order =
 * `Done.Operations` order = the matcher's plan order), so mrpc no longer carries its own
 * `Symbol.newClass` proxy macro.
 *
 * Arity routing: `Unit` -> `fire`; `Future[X]` -> `call` (result decoded via `forFuture`); sub-RPC ->
 * `get` (a SINGLE summon seam, lazily recursive via `AsReal.makeLazy` so self/mutually-referential
 * sub-RPCs derive without infinite inline expansion).
 */
object AsRealDerivation:

  /**
   * Builds the client proxy `AsReal[RawRpc[Raw], Real]`. `ec` is threaded as a `using` parameter so the
   * `call` arity can compose the result decoder `AsReal[Future[Raw], Future[r]]` via `forFuture`.
   */
  def impl[Raw: Type, Real: Type](done: Expr[Done.Of[Real]], ec: Expr[ExecutionContext])(using Quotes)
    : Expr[AsReal[RawRpc[Raw], Real]] =
    val plans: List[OpPlan] = Matcher.planAll[Real](done)
    '{
      new AsReal[RawRpc[Raw], Real]:
        def asReal(raw: RawRpc[Raw]): Real =
          ${ materializeProxy[Raw, Real]('raw, plans, ec, done) }
    }

  /**
   * Builds the proxy via made's `Done.materialize`: one handler per plan — shaped to match made's
   * [[Done.HandlerOf]] (a no-param op expects `() => OutputType`; a parametric op expects a
   * `NamedTuple => OutputType`) — collected into a tuple ascribed to the EXACT `Done.HandlersOf[ops]`
   * type, then `.to[Real]`.
   *
   * The tuple is ascribed (`asInstanceOf[hs]`) to the concrete handler-tuple type computed from the
   * mirror's `Operations`, then `to` is given the mirror plus `made.ValidHandlers.refl` as its
   * handler/operation correspondence evidence. mrpc builds exactly one handler per operation, in
   * `Done.Operations` order, each shaped to `Done.HandlerOf[op]` (see `handlerFor`), so the handler
   * tuple IS `Done.HandlersOf[Operations]` by construction — `refl[mirror.Operations, hs]` is the
   * macro-side witness for that, used in place of the auto-`given`
   * (`ValidHandlers[Ops, Done.HandlersOf[Ops]]`) whose `Done.HandlersOf` match type does not reduce
   * during this macro's own expansion. Pinning both `to` arguments to the one `mirror` path makes the
   * evidence the slot requires (`ValidHandlers[mirror.Operations, hs]`) syntactically what `refl`
   * supplies, so no reduction is needed.
   */
  private def materializeProxy[Raw: Type, Real: Type](
    raw: Expr[RawRpc[Raw]],
    plans: List[OpPlan],
    ec: Expr[ExecutionContext],
    done: Expr[Done.Of[Real]],
  )(using Quotes,
  ): Expr[Real] =
    val handlers: List[Expr[Any]] = plans.map(plan => handlerFor[Raw](raw, plan, ec))
    val handlersTuple: Expr[Tuple] = Expr.ofTupleFromSeq(handlers)
    handlersTupleType[Real](done) match
      case '[type hs <: Tuple; hs] =>
        '{
          val mirror = Done.derived[Real]
          $handlersTuple.asInstanceOf[hs].to[Real](using mirror)(using ValidHandlers.refl[mirror.Operations, hs])
        }
      case _ => quotes.reflect.report.errorAndAbort("handler tuple type is not a Tuple")

  /**
   * The concrete `TupleN[Done.HandlerOf[op1], ..., Done.HandlerOf[opN]]` type for the mirror's
   * operations. This is structurally `Done.HandlersOf[Operations]` (the contract made's `to` checks),
   * but materialized as a concrete `TupleN` rather than the unreduced `Tuple.Map` match type: made's
   * `materializeImpl` indexes the handler tuple via `Handlers.typeSymbol.fieldMember("_i")`, which only
   * resolves on a concrete `TupleN`. Each element is the reduced `Done.HandlerOf[op]` (a `() => Out` or
   * `NamedTuple => Out` function type), matching the handlers `handlerFor` builds.
   */
  private def handlersTupleType[Real: Type](done: Expr[Done.Of[Real]])(using Quotes): Type[?] =
    import quotes.reflect.*
    val ops: List[Type[?]] = Matcher.operationTypes[Real](done)
    val elemReprs: List[TypeRepr] = ops.map { case '[o] => TypeRepr.of[Done.HandlerOf[o]].dealias }
    val n = elemReprs.size
    val tupleRepr =
      if n == 0 then TypeRepr.of[EmptyTuple]
      else if n <= 22 then defn.TupleClass(n).typeRef.appliedTo(elemReprs)
      else elemReprs.foldRight(TypeRepr.of[EmptyTuple])((h, acc) => TypeRepr.of[*:].appliedTo(List(h, acc)))
    tupleRepr.asType

  /**
   * One operation's handler, shaped to made's [[Done.HandlerOf]]: a no-param op becomes `() =>
   * OutputType`; a parametric op becomes `<args> => OutputType` where `<args>` is the operation's
   * (named) argument tuple. made invokes the handler with the argument tuple, which is destructured
   * back into positional argument terms (each cast to its exact param type), encoded, packaged into a
   * [[RawInvocation]], and dispatched by arity.
   */
  private def handlerFor[Raw: Type](
    raw: Expr[RawRpc[Raw]],
    plan: OpPlan,
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[Any] =
    val outTpe: Type[?] = OpReflect.outputType(plan.opType)
    if plan.params.isEmpty then
      outTpe match
        case '[o] =>
          '{ () => ${ handlerBody[Raw, EmptyTuple]('{ EmptyTuple }, plan, raw, ec).asExprOf[o] } }
    else
      val argsTpe = tupleTypeOf(plan.params.map(_.paramType))
      argsTpe.asType match
        case '[type a <: Tuple; a] =>
          outTpe match
            case '[o] =>
              '{ (args: a) => ${ handlerBody[Raw, a]('args, plan, raw, ec).asExprOf[o] } }

  /** Builds the `op.Args` tuple type from the flattened param types (`t1 *: ... *: EmptyTuple`). */
  private def tupleTypeOf(tpes: List[Type[?]])(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    tpes.foldRight(TypeRepr.of[EmptyTuple]) { (t, acc) =>
      (t, acc.asType) match
        case ('[x], '[type r <: Tuple; r]) => TypeRepr.of[x *: r]
        case _ => acc
    }

  /**
   * The body of one handler: package a [[RawInvocation]] (resolved `rpcName` + nested encoded args) and
   * forward to `raw.fire`/`raw.call`/`raw.get` by the planned arity, decoding the result to the op's
   * exact declared `OutputType`.
   */
  private def handlerBody[Raw: Type, A <: Tuple: Type](
    args: Expr[A],
    plan: OpPlan,
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[Any] =
    import quotes.reflect.*

    // Recover positional argument terms from the args tuple, each cast to its exact declared type, so
    // the per-param `AsRaw[Raw, t]` encoder applies as the source would.
    val flatArgTerms: List[Term] = plan.params.zipWithIndex.map { case (param, i) =>
      param.paramType match
        case '[t] => '{ $args.productElement(${ Expr(i) }).asInstanceOf[t] }.asTerm
    }

    val invocation = invocationExpr[Raw](plan, flatArgTerms)

    plan.arity match
      case Arity.Fire =>
        '{ $raw.fire($invocation) }
      case Arity.Call(resultType) =>
        resultType match
          case '[r] =>
            '{
              val futureDecoder: AsReal[Future[Raw], Future[r]] =
                AsReal.forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], $ec)
              futureDecoder.asReal($raw.call($invocation))
            }
      case Arity.Get(subRpcType) =>
        subRpcType match
          case '[sub] =>
            '{
              val subProxy = compiletime.summonInline[AsReal[RawRpc[Raw], sub]]
              AsReal.makeLazy[RawRpc[Raw], sub](subProxy).asReal($raw.get($invocation))
            }

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
    val encodedArgs: List[Expr[Raw]] = plan.params.zip(flatArgTerms).map { case (param, argTerm) =>
      param.paramType match
        case '[t] =>
          '{ scala.compiletime.summonInline[AsRaw[Raw, t]].asRaw(${ argTerm.asExprOf[t] }) }
    }

    val sizes = OpReflect.paramListSizes(plan.opType)
    val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
    val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
    val argsExpr: Expr[List[List[Raw]]] =
      if nested.forall(_.isEmpty) then '{ Nil } else Expr.ofList(nestedExprs)

    '{ RawInvocation[Raw](${ Expr(plan.rpcName) }, $argsExpr) }

  /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
  private def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
    sizes
      .foldLeft((remaining = items, acc = List.empty[List[A]])) { case ((remaining, acc), n) =>
        val (group, rest) = remaining.splitAt(n)
        (rest, group :: acc)
      }
      .acc
      .reverse
