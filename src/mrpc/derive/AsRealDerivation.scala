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
   * The client-proxy conversion as a plain value: `asReal` turns a `RawRpc[Raw]` into a `Real` proxy
   * via the [[materializeProxy]] macro. The `AsReal` wrapper itself is ordinary Scala; only the
   * per-`raw` proxy body is generated. `ExecutionContext` is in scope so the `call` arity can compose
   * `AsReal[Future[Raw], Future[r]]` via `forFuture`.
   */
  inline def impl[Raw, Real: Done.Of](using ExecutionContext): AsReal[RawRpc[Raw], Real] =
    (raw: RawRpc[Raw]) => materializeProxy[Raw, Real](raw)

  /**
   * Macro entry: build the `Real` client proxy for a single `RawRpc[Raw]`. The mirror and
   * `ExecutionContext` are summoned here and handed to [[materializeProxyImpl]].
   */
  inline def materializeProxy[Raw, Real: {Done.Of as done, RpcNames as names}](
    raw: RawRpc[Raw],
  )(using ec: ExecutionContext,
  ): Real =
    ${ materializeProxyImpl[Raw, Real]('raw, 'done, 'names, 'ec) }

  /**
   * Builds the proxy via made's `Done.materialize`: one handler per plan — shaped to match made's
   * [[Done.HandlerOf]] (a no-param op expects `() => OutputType`; a parametric op expects a
   * `NamedTuple => OutputType`) — collected via [[Expr.ofRefinedTuple]] into a precisely-typed
   * heterogeneous tuple and handed to `.to[Real]`.
   *
   * made's `materializeImpl` reads handler `i` via `productElement(i)` rather than a
   * `Handlers`-typed symbol lookup, so the handler tuple no longer needs to be ascribed to a concrete
   * `TupleN` for that to work — `ofRefinedTuple`'s natural per-element-precise `*:`-chain is enough.
   * `to` is still given `made.ValidHandlers.refl` as its handler/operation correspondence evidence
   * (rather than relying on the auto-`given ValidHandlers[Ops, H](using H <:< Done.HandlersOf[Ops])`):
   * even with `Handlers` now correctly inferred as the precise `hs`, `Done.HandlersOf[mirror.Operations]`
   * doesn't itself reduce far enough for that implicit search to succeed, since it's a match type over
   * the still-abstract, path-dependent `mirror.Operations` — the same wall `OpReflect` exists to route
   * around elsewhere. `refl[mirror.Operations, hs]` is the macro-side witness for that instead: mrpc
   * builds exactly one handler per operation, in `Done.Operations` order, each shaped to
   * `Done.HandlerOf[op]` (see `handlerFor`), so the handler tuple IS `Done.HandlersOf[Operations]` by
   * construction.
   */
  private def materializeProxyImpl[Raw: Type, Real: Type](
    raw: Expr[RawRpc[Raw]],
    done: Expr[Done.Of[Real]],
    names: Expr[RpcNames[Real]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[Real] =
    val plans = Matcher.plans[Real](done, names)
    val handlers: List[Expr[Any]] = plans.map(plan => handlerFor[Raw](raw, plan, ec))
    // `ofRefinedTuple` keeps each handler's own precise type through the fold, so pattern-matching its
    // result back to a bound type `hs` (rather than stashing it in a `val: Expr[Tuple]`, which would
    // widen it right back down) lets the quote below see the handler tuple at that PRECISE type —
    // `.to[Real]`'s `Handlers` type parameter is correctly inferred as `hs`, not a widened `Tuple`.
    // `Done.HandlersOf[mirror.Operations]` still doesn't itself reduce far enough for the implicit
    // search backing the auto-`given ValidHandlers` to see `hs <:< HandlersOf[Operations]` — that match
    // type is over the path-dependent, still-abstract `mirror.Operations`, the same wall `OpReflect`
    // exists to route around elsewhere — so `refl[mirror.Operations, hs]` is still needed as the
    // macro-side witness. It is at least honest now: `hs` is what the handlers actually build, not a
    // stand-in `Tuple`.
    Expr.ofRefinedTuple(handlers) match
      case '{ type hs <: Tuple; $handlersTuple: hs } =>
        '{
          // Reuses the SAME mirror `plans` above was already classified from — no need to
          // re-derive one via `Done.derived[Real]`. Bound to a local `val` (rather than projecting
          // off `$done` directly) so `.Operations` is a stable path `to`/`refl` can reference; `$done`
          // itself need not denote a stable path (e.g. it may be a `summon[Done.Of[Real]]` call).
          val mirror: Done.Of[Real] = $done
          $handlersTuple.to[Real](using mirror)(using ValidHandlers.refl[mirror.Operations, hs])
        }

  /**
   * One operation's handler, shaped EXACTLY to made's [[Done.HandlerOf]]: a no-param op becomes `() =>
   * OutputType`; a parametric op becomes `NamedTuple[names, types] => OutputType`, built via
   * [[namedArgsType]] so the lambda's parameter is precisely the shape `Done.HandlerOf[op]` expects,
   * not a widened `Tuple` cast away. made invokes the handler with the argument tuple, which is
   * destructured back into positional argument terms (each cast to its exact param type), encoded,
   * packaged into a [[RawInvocation]], and dispatched by arity.
   */
  private def handlerFor[Raw: Type](
    raw: Expr[RawRpc[Raw]],
    plan: Type[?],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[?] =
    if paramsOf(plan).isEmpty then '{ () => ${ handlerBody[Raw, EmptyTuple]('{ EmptyTuple }, plan, raw, ec) } }
    else
      // No bound (`<: Product`/`<: Tuple`) on `argsT` here: a `NamedTuple` built from these
      // quote-pattern-bound (skolem) `n`/`v` type variables fails such bound checks in a quote
      // pattern, even though the SAME subtyping holds for ordinary, non-macro code — so `argsT` is
      // left unbounded and `handlerBody` reaches into it via an explicit `asInstanceOf[Product]`.
      namedArgsType(plan) match
        case '[argsT] =>
          '{ (args: argsT) => ${ handlerBody[Raw, argsT]('args, plan, raw, ec) } }

  /**
   * The op's argument type, exactly as made's [[Done.HandlerOf]] computes it: a `NamedTuple` keyed by
   * each param's label (a singleton-string tuple, same `ConstantType(StringConstant(...))` idiom
   * [[Matcher.planOne]] uses), valued by each param's declared type.
   */
  private def namedArgsType(plan: Type[?])(using Quotes): Type[?] =
    import quotes.reflect.*
    val params = paramsOf(plan)
    val namesType = TupleTraverse.foldTuple(params.map(p => ConstantType(StringConstant(p.label)).asType))
    val typesType = TupleTraverse.foldTuple(params.map(_.paramType))
    (namesType, typesType) match
      case ('[type n <: Tuple; n], '[type v <: Tuple; v]) => Type.of[scala.NamedTuple.NamedTuple[n, v]]
      case _ => report.errorAndAbort(s"could not build named-tuple arg type for ${TypeRepr.of(using plan).show}")

  /** One parameter's label + declared type, read off `plan`'s `Params` tuple, in order. */
  private def paramsOf(plan: Type[?])(using Quotes): List[(label: String, paramType: Type[?])] =
    (plan.runtimeChecked match
      case '[OpPlan { type Params = ps }] => Type.of[ps]
    ) match
      case '[type pst <: Tuple; pst] =>
        TupleTraverse.traverseTuple(Type.of[pst]).map { pt =>
          pt.runtimeChecked match
            case '[type l <: String; ParamPlan { type Label = l; type ParamType = t }] =>
              val label = Type
                .valueOfConstant[l]
                .getOrElse(quotes.reflect.report.errorAndAbort("Label is not a string literal"))
                .toString
              (label = label, paramType = Type.of[t])
        }

  /**
   * The body of one handler: package a [[RawInvocation]] (resolved `rpcName` + nested encoded args) and
   * forward to `raw.fire`/`raw.call`/`raw.get` by the planned arity, decoding the result to the op's
   * exact declared `OutputType`.
   */
  private def handlerBody[Raw: Type, A: Type](
    args: Expr[A],
    plan: Type[?],
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
  )(using Quotes,
  ): Expr[?] =
    // Recover positional argument terms from the args tuple, each cast to its exact declared type, so
    // the per-param `AsRaw[Raw, t]` encoder applies as the source would. `A` carries no `Product`
    // bound (see `handlerFor`), so `args` is cast to `Product` here — safe, since every `A` this is
    // called with (`EmptyTuple` or a `NamedTuple`) IS one at runtime.
    val flatArgTerms: List[Expr[?]] = paramsOf(plan).zipWithIndex.map { case (param, i) =>
      param.paramType match
        case '[t] =>
          '{ $args.asInstanceOf[Product].productElement(${ Expr(i) }).asInstanceOf[t] }
    }

    val invocation = invocationExpr[Raw](plan, flatArgTerms)

    val arityInfo: Type[?] = plan.runtimeChecked match
      case '[OpPlan { type ArityInfo = a }] => Type.of[a]
    arityInfo match
      case '[ArityTag.Fire] =>
        '{ $raw.fire($invocation) }
      case '[ArityTag.CallOf[r]] =>
        '{
          val futureDecoder: AsReal[Future[Raw], Future[r]] =
            AsReal.forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], $ec)
          futureDecoder.asReal($raw.call($invocation))
        }
      case '[ArityTag.GetOf[sub]] =>
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
    using quotes: Quotes,
  )(
    plan: Type[?],
    flatArgTerms: List[Expr[Any]],
  ): Expr[RawInvocation[Raw]] =
    import quotes.reflect.*
    val encodedArgs: List[Expr[Raw]] = paramsOf(plan).zip(flatArgTerms).map { case (param, argTerm) =>
      param.paramType match
        case '[t] =>
          '{ scala.compiletime.summonInline[AsRaw[Raw, t]].asRaw(${ argTerm.asExprOf[t] }) }
    }

    val opType = plan.runtimeChecked match
      case '[OpPlan { type OpType = o }] => Type.of[o]
    val sizes = OpReflect.paramListSizes(opType)
    val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
    val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
    val argsExpr: Expr[List[List[Raw]]] =
      if nested.forall(_.isEmpty) then '{ Nil } else Expr.ofList(nestedExprs)

    val rpcName = plan.runtimeChecked match
      case '[type n <: String; OpPlan { type RpcName = n }] =>
        Type.valueOfConstant[n].getOrElse(report.errorAndAbort("RpcName is not a string literal"))

    '{ RawInvocation[Raw](${ Expr(rpcName) }, $argsExpr) }

  /** Splits `items` into consecutive groups of the given `sizes` (the inverse of `flatten`). */
  private def splitBySizes[A](items: List[A], sizes: List[Int]): List[List[A]] =
    sizes
      .foldLeft((remaining = items, acc = List.empty[List[A]])) { case ((remaining, acc), n) =>
        val (group, rest) = remaining.splitAt(n)
        (rest, group :: acc)
      }
      .acc
      .reverse
