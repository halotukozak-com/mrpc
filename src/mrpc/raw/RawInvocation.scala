package mrpc.raw

/**
 * A single raw invocation: resolved rpc name + per-param-list raw arguments + call metadata.
 * `args` is nested (List[List[Raw]]) so each inner list is one parameter list, aligning with
 * the per-param-list arity shape consumed by later derivation.
 *
 * `Raw` is a fully abstract type parameter: it IS the opaque serialized-value carrier, with no
 * transport or format API leaking into the raw layer. No concrete or opaque carrier type exists.
 */
final case class RawInvocation[Raw](
  rpcName: String,
  args: List[List[Raw]],
  metadata: Map[String, String] = Map.empty,
)
