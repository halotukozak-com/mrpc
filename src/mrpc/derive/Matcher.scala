package mrpc.derive

import scala.quoted.*

/**
 * Reads [[Plans]] — the single classification authority ([[OpPlan.classify]], collected once per `T`)
 * — back into the shapes callers need: the full ordered list ([[plans]]), or a lookup by label
 * ([[planFor]] / [[plansFor]]), for tests and for the server-adapter dispatch build ([[AsRawDerivation]]).
 *
 * Deliberately does NOT classify anything itself: every op's [[OpPlan]] already exists as a member of
 * `Plans[T].All`, so this object only ever filters/looks up that one tuple — no separate
 * re-derivation path to keep in sync with [[OpPlan.impl]].
 */
private[mrpc] object Matcher:

  /** The op's (or param's) `Label` singleton-string member as a plain `String`. */
  private def labelOf(opType: Type[?])(using Quotes): String =
    opType.runtimeChecked match
      case '[type l <: String; { type Label = l }] =>
        Type.valueOfConstant[l].getOrElse(quotes.reflect.report.errorAndAbort("Label is not a string literal")).toString

  /**
   * Every operation of `T`, classified — one per `Done.Operations` entry, in the same order — the
   * consumer-facing list the server adapter iterates over, each queried on demand via local
   * quote-pattern reads on the plan's `Type[?]`. Sourced from [[Plans]] (summoned once at the call
   * site via the `Plans as plans` context bound), not re-derived here.
   */
  def plans[T: Type](plans: Expr[Plans[T]])(using Quotes): List[Type[? <: OpPlan]] =
    Plans.allOf[T](plans)

  /**
   * Exposes a SINGLE operation's [[OpPlan]] type directly to the CALLER's type checker —
   * `transparent inline`, the same technique `made.Done.derived` uses to make its refined type
   * visible outside the macro. Lets ordinary, non-macro code (tests) assert compile-time facts about
   * one operation's classification, e.g. `summon[Matcher.planFor[SampleApi, "ping"].ArityInfo =:=
   * ArityTag.Fire]`, instead of comparing a runtime value with `assertEquals`. The returned value is a
   * `null` dummy: nothing here is ever meant to be called/dereferenced at runtime, only its STATIC
   * type — a plain type projection — is used.
   */
  transparent inline def planFor[T: {Plans as plans}, L <: String]: OpPlan =
    ${ planForImpl[T, L]('plans) }

  private def planForImpl[T: Type, L: Type](plans: Expr[Plans[T]])(using Quotes): Expr[OpPlan] =
    import quotes.reflect.*
    val all = Plans.allOf[T](plans)
    val label = Type.valueOfConstant[L].getOrElse(report.errorAndAbort("L must be a literal string")).toString
    val idx = all.indexWhere(op => labelOf(op) == label)
    if idx < 0 then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    all(idx) match
      case '[type p <: OpPlan; p] => '{ null.asInstanceOf[p] }

  /**
   * Like [[planFor]], but returns EVERY operation sharing label `L` as a tuple (declaration order) —
   * needed when a label is shared by more than one op (overloads), to compare their [[OpPlan]]s
   * against each other at the type level (e.g. asserting their resolved `RpcName`s differ via
   * `scala.util.NotGiven`). `transparent inline`, so destructuring the tuple (`val (a, b) =
   * plansFor[...]`) gives each bound val its OWN precise `OpPlan` type, same as [[planFor]] does for a
   * single operation.
   */
  transparent inline def plansFor[T: {Plans as plans}, L <: String]: Tuple =
    ${ plansForImpl[T, L]('plans) }

  private def plansForImpl[T: Type, L: Type](plans: Expr[Plans[T]])(using Quotes): Expr[Tuple] =
    import quotes.reflect.*
    val all = Plans.allOf[T](plans)
    val label = Type.valueOfConstant[L].getOrElse(report.errorAndAbort("L must be a literal string")).toString
    val matches = all.filter(op => labelOf(op) == label)
    if matches.isEmpty then report.errorAndAbort(s"no operation labeled '$label' in ${TypeRepr.of[T].show}")
    val nulls: List[Expr[Any]] = matches.map { case '[type p <: OpPlan; p] => '{ null.asInstanceOf[p] } }
    Expr.ofRefinedTuple(nulls)
