package mrpc.derive

/**
 * Type-level tag for which `RawRpc` dispatch method (`fire`/`call`/`get`) an operation routes to.
 * `Call`/`Get` carry their payload (the `Future` result type / the sub-RPC type) as a type member, so
 * it survives as part of an [[OpPlan]]'s own type. The matcher, the two materialize macros, and
 * [[Matcher.describe]]'s test-facing projection all classify arity by quote-pattern-matching this
 * type directly (`case '[ArityTag.Fire] => ...` / `case '[ArityTag.CallOf[r]] => ...`) — there is no
 * separate value-level enum to keep in sync with it.
 */
private[derive] sealed trait ArityTag
private[derive] object ArityTag:
  sealed trait Fire extends ArityTag
  sealed trait Call extends ArityTag:
    type Result
  sealed trait Get extends ArityTag:
    type Sub

  type CallOf[R] = Call { type Result = R }
  type GetOf[S] = Get { type Sub = S }

/**
 * Type-level encode-vs-verbatim tag, classified the same way as [[ArityTag]] — via quote-pattern
 * matching directly on the type, no value-level enum counterpart.
 */
private[derive] sealed trait EncodingTag
private[derive] object EncodingTag:
  sealed trait Encoded extends EncodingTag
  sealed trait Verbatim extends EncodingTag

/**
 * One parameter's plan, at the type level: its label, declared type, and encode-vs-verbatim tag.
 * [[OpPlan.Params]] is a `Tuple` of these — the parameter-level counterpart of [[OpPlan]] itself.
 */
private[derive] sealed trait ParamPlan:
  type Label <: String
  type ParamType
  type Encoding <: EncodingTag

/**
 * One operation's classification, at the type level: arity, resolved rpcName, per-parameter encode
 * plan, and the underlying `DoneOperation` this plan describes. [[Matcher.planAll]] builds a
 * `Plans <: Tuple` of these — one per `Done.Operations` entry, in the same order, so a plan's
 * position in `Plans` (equivalently, in [[Matcher.plans]]'s resulting list) IS its `Done.Operations`
 * index. Mirrors how made's `Done` models `T` as a `Tuple` of `DoneOperation`s; read back via
 * [[PlanReflect]].
 */
private[derive] sealed trait OpPlan:
  type Label <: String
  type RpcName <: String
  type ArityInfo <: ArityTag
  type Params <: Tuple
  type ResultEncoding <: EncodingTag
  type OpType

/**
 * Flattened, runtime-comparable projection of one [[OpPlan]] — the test-facing output of the matcher.
 * Because an [[OpPlan]] is a TYPE (carrying `Type[?]`-only information), the suites assert against
 * this plain case class instead (arity rendered as a tag, the carried result/sub-RPC type as its
 * `show` string, and per-param encodings as comparable strings) — see [[Matcher.describe]].
 */
final case class OpDescriptor(
  label: String,
  rpcName: String,
  arity: String,
  carriedType: String,
  paramEncodings: List[String],
  resultEncoding: String,
)
