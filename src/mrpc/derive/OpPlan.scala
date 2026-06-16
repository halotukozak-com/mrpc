package mrpc.derive

import scala.quoted.*

/**
 * Compile-time-only data model produced by the matcher: the standalone, independently-testable
 * classification of one operation into its arity, resolved rpcName, and per-parameter encode plan.
 *
 * These case classes carry `Type[?]`/`String` values that exist only inside a macro expansion — they
 * are NOT runtime data types. The matcher builds an `OpPlan` per operation; the server adapter and
 * client proxy macros (later) consume it to summon codecs and assemble dispatch. For test assertions
 * the matcher also exposes a flattened runtime [[OpDescriptor]] (see [[Matcher.describe]]).
 */
private[derive] enum Arity:
  case Fire
  case Call(resultType: Type[?])
  case Get(subRpcType: Type[?])

private[derive] enum Encoding:
  case Encoded
  case Verbatim

private[derive] final case class ParamPlan(
  label: String,
  paramType: Type[?],
  encoding: Encoding,
)

private[derive] final case class OpPlan(
  label: String,
  rpcName: String,
  arity: Arity,
  params: List[ParamPlan],
  resultEncoding: Encoding,
  // The operation's refined `DoneOperation` type and its position in `Done.Operations`. The
  // materialize macros need both to select the exact operation term off the mirror and call the
  // type-safe `Done.invoke`. They are macro-only payloads (like `paramType`), absent from the
  // runtime `OpDescriptor` projection.
  opType: Type[?],
  index: Int,
)

/**
 * Flattened, runtime-comparable projection of an [[OpPlan]] — the test-facing output of the matcher.
 * Because `OpPlan` carries `Type[?]` values that only exist during macro expansion, the suites assert
 * against this plain case class instead (arity rendered as a tag, the carried result/sub-RPC type as
 * its `show` string, and per-param encodings as comparable strings).
 */
final case class OpDescriptor(
  label: String,
  rpcName: String,
  arity: String,
  carriedType: String,
  paramEncodings: List[String],
  resultEncoding: String,
)
