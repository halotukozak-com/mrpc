package halotukozak.mrpc.annotation

import made.annotation.MetaAnnotation

/** Base for all RPC tag annotations. Mirrors commons `RpcTag`; the commons base-trait hierarchy is otherwise dropped. */
trait RpcTag extends MetaAnnotation

// Commons defaults `defaultTag`/`whenUntagged` to a bare reference default; here they are
// `Option[..] = None` instead (deliberate divergence — the no-bare-reference style rule).

/** Declares the tag type for an RPC's methods, with an optional default. Mirrors commons `methodTag[BaseTag](defaultTag)`. */
final class methodTag[BaseTag <: RpcTag](val defaultTag: Option[BaseTag] = None) extends MetaAnnotation

/** Declares the tag type for an RPC method's params, with an optional default. Mirrors commons `paramTag[BaseTag](defaultTag)`. */
final class paramTag[BaseTag <: RpcTag](val defaultTag: Option[BaseTag] = None) extends MetaAnnotation

/** Tags a method or param with a concrete tag type, with an optional value used when untagged. Mirrors commons `tagged[Tag](whenUntagged)`. */
final class tagged[Tag <: RpcTag](val whenUntagged: Option[Tag] = None) extends MetaAnnotation
