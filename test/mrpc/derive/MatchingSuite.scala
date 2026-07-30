package mrpc.derive

import mrpc.derive.SampleApi.*

/**
 * Drives the standalone matcher (op -> OpPlan) against SampleApi: arity classification from the
 * output type and the per-param encode-vs-verbatim plan under abstract `Raw`.
 *
 * Checked entirely at COMPILE TIME: `Matcher.planFor[T, "label"]` exposes one operation's `OpPlan`
 * type directly to this ordinary (non-macro) test file via `transparent inline` — the same technique
 * `made.Done.derived` uses to make its refined type visible outside the macro that built it. Each
 * call is bound to a local `val` (a stable path is required to project its type members in type
 * position, e.g. `ping.ArityInfo`), and `summon[... =:= ...]`/`summon[... <:< ...]` prove facts about
 * it. No runtime `OpDescriptor` values are compared — a wrong fact here fails the whole file to
 * COMPILE, not one test case at run time.
 */
class MatchingSuite extends munit.FunSuite:

  test("arity is classified from the output type (Unit->fire, Future[X]->call, sub-RPC->get)"):
    val ping = Matcher.planFor[SampleApi, "ping"]
    val increment = Matcher.planFor[SampleApi, "increment"]
    val find = Matcher.planFor[SampleApi, "find"]
    val users = Matcher.planFor[SampleApi, "users"]
    summon[ping.ArityInfo =:= ArityTag.Fire]
    summon[increment.ArityInfo <:< ArityTag.Call]
    summon[find.ArityInfo <:< ArityTag.Call]
    summon[users.ArityInfo <:< ArityTag.Get]

  test("call arity carries its result type; get arity carries the sub-RPC type"):
    val increment = Matcher.planFor[SampleApi, "increment"]
    val find = Matcher.planFor[SampleApi, "find"]
    val users = Matcher.planFor[SampleApi, "users"]
    summon[increment.ArityInfo =:= ArityTag.CallOf[Int]]
    summon[find.ArityInfo =:= ArityTag.CallOf[User]]
    summon[users.ArityInfo =:= ArityTag.GetOf[UsersRpc]]

  test("each value param gets an encode-vs-verbatim plan (every value param encoded under abstract Raw)"):
    val increment = Matcher.planFor[SampleApi, "increment"]
    val echoBool = Matcher.planFor[SampleApi, "echoBool"]
    val combine = Matcher.planFor[SampleApi, "combine"]
    val ping = Matcher.planFor[SampleApi, "ping"]
    // increment(n: Int) -> one encoded param; echoBool(b: Boolean) -> one encoded param.
    summon[
      increment.Params =:=
        (ParamPlan { type Label = "n"; type ParamType = Int; type Encoding = EncodingTag.Encoded } *: EmptyTuple),
    ]
    summon[
      echoBool.Params =:=
        (ParamPlan {
          type Label = "b"; type ParamType = Boolean; type Encoding = EncodingTag.Encoded
        } *: EmptyTuple),
    ]
    // combine(a: Int)(b: String, c: Long) flattens to three encoded params.
    summon[
      combine.Params =:= (
        ParamPlan { type Label = "a"; type ParamType = Int; type Encoding = EncodingTag.Encoded } *: ParamPlan {
          type Label = "b"; type ParamType = String; type Encoding = EncodingTag.Encoded
        } *: ParamPlan { type Label = "c"; type ParamType = Long; type Encoding = EncodingTag.Encoded } *: EmptyTuple,
      ),
    ]
    // ping() has no value params.
    summon[ping.Params =:= EmptyTuple]

  test("default result encoding is encoded under abstract Raw"):
    val find = Matcher.planFor[SampleApi, "find"]
    val increment = Matcher.planFor[SampleApi, "increment"]
    summon[find.ResultEncoding =:= EncodingTag.Encoded]
    summon[increment.ResultEncoding =:= EncodingTag.Encoded]
