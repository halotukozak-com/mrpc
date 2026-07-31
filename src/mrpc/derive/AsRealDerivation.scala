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
  inline def materializeProxy[Raw, Real: {Done.Of as done, RpcNames as names, Plans as plans}](
    raw: RawRpc[Raw],
  )(using ec: ExecutionContext,
  ): Real =
    compiletime
      .summonAll[Tuple.Map[plans.type, [Op] =>> Handler[Raw, Op]]]
      .to[Real](using done)(using ValidHandlers.refl)

  /**
   * One operation's handler, shaped EXACTLY to made's [[Done.HandlerOf]]: a no-param op becomes `() =>
   * OutputType`; a parametric op becomes `NamedTuple[names, types] => OutputType`, built via
   * [[namedArgsType]] so the lambda's parameter is precisely the shape `Done.HandlerOf[op]` expects,
   * not a widened `Tuple` cast away. made invokes the handler with the argument tuple, which is
   * destructured back into positional argument terms (each cast to its exact param type), encoded,
   * packaged into a [[RawInvocation]], and dispatched by arity.
   */
  private def handlerFor[Raw: Type, Plan <: OpPlan: Type](
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
    lists: Expr[List[Int]],
  )(using Quotes,
  ): Expr[?] =
    if paramsOf[Plan].isEmpty then '{ () => ${ handlerBody[Raw, Plan]('{ EmptyTuple }, raw, ec, lists) } }
    else
      TupleTraverse.foldTuple(paramsOf[Plan].map(_.paramType)) match
        case '[type args <: Tuple; args] =>
          '{ (args: args) => ${ handlerBody[Raw, Plan]('args, raw, ec, lists) } }

  /**
   * The op's argument type, exactly as made's [[Done.HandlerOf]] computes it: a `NamedTuple` keyed by
   * each param's label (a singleton-string tuple, same `ConstantType(StringConstant(...))` idiom
   * [[Matcher.planOne]] uses), valued by each param's declared type.
   */

  /** One parameter's label + declared type, read off `plan`'s `Params` tuple, in order. */
  private def paramsOf[Plan <: OpPlan: Type](using Quotes): List[(label: String, paramType: Type[?])] =
    Type.of[Plan] match
      case '[type ps <: Tuple; OpPlan { type Params = ps }] =>
        TupleTraverse.traverseTuple[ps, ParamPlan].map {
          case '[type l <: String; ParamPlan {
                type Label = l
                type ParamType = t
              }] =>
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
  private def handlerBody[Raw: Type, Plan <: OpPlan: Type](
    args: Expr[? <: Tuple],
    raw: Expr[RawRpc[Raw]],
    ec: Expr[ExecutionContext],
    sizes: Expr[List[Int]],
  )(using Quotes,
  ): Expr[?] =
    // Recover positional argument terms from the args tuple, each cast to its exact declared type, so
    // the per-param `AsRaw[Raw, t]` encoder applies as the source would. `A` carries no `Product`
    // bound (see `handlerFor`), so `args` is cast to `Product` here — safe, since every `A` this is
    // called with (`EmptyTuple` or a `NamedTuple`) IS one at runtime.
    val flatArgTerms: List[Expr[?]] = paramsOf[Plan].zipWithIndex.map { case (param, i) =>
      param.paramType match
        case '[t] =>
          '{ $args(${ Expr(i) }).asInstanceOf[t] }
    }

    val invocation = invocationExpr[Raw, Plan](flatArgTerms)

    Type.of[Plan] match
      case '[{ type ArityInfo = ArityTag.Fire }] =>
        '{ $raw.fire($invocation) }
      case '[{ type ArityInfo = ArityTag.CallOf[r] }] =>
        '{
          val futureDecoder: AsReal[Future[Raw], Future[r]] =
            AsReal.forFuture[Raw, r](using scala.compiletime.summonInline[AsReal[Raw, r]], $ec)
          futureDecoder.asReal($raw.call($invocation))
        }
      case '[{ type ArityInfo = ArityTag.GetOf[sub] }] =>
        '{
          val subProxy = compiletime.summonInline[AsReal[RawRpc[Raw], sub]]
          AsReal.makeLazy[RawRpc[Raw], sub](subProxy).asReal($raw.get($invocation))
        }

  /**
   * Builds `RawInvocation(<rpcName>, <nested encoded args>)`. Each argument is encoded to `Raw` via a
   * summoned `AsRaw[Raw, paramType]`, then the flat encoded list is split back into `List[List[Raw]]`
   * per the op's `ParamLists` sizes — the exact inverse of the server adapter's `args.flatten`.
   */
  private def invocationExpr[Raw: Type, Plan <: OpPlan: Type](
    using quotes: Quotes,
  )(
    flatArgTerms: List[Expr[Any]],
  ): Expr[RawInvocation[Raw]] =
    import quotes.reflect.*
    val encodedArgs: List[Expr[Raw]] = paramsOf[Plan].zip(flatArgTerms).map { case (param, argTerm) =>
      param.paramType match
        case '[t] =>
          '{ scala.compiletime.summonInline[AsRaw[Raw, t]].asRaw(${ argTerm.asExprOf[t] }) }
    }

    val opType = Type.of[Plan] match
      case '[OpPlan { type OpType = o }] => Type.of[o]
    val sizes = OpReflect.paramListSizes(opType)
    val nested: List[List[Expr[Raw]]] = splitBySizes(encodedArgs, sizes)
    val nestedExprs: List[Expr[List[Raw]]] = nested.map(inner => Expr.ofList(inner))
    val argsExpr: Expr[List[List[Raw]]] =
      if nested.forall(_.isEmpty) then '{ Nil } else Expr.ofList(nestedExprs)

    val rpcName = Type.of[Plan] match
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
