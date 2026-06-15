package mrpc.annotation

import made.annotation.MetaAnnotation

/** Name prefix for RPC methods. Mirrors commons `rpcNamePrefix(prefix, overloadedOnly = false)`. */
final class rpcNamePrefix(val prefix: String, val overloadedOnly: Boolean = false) extends MetaAnnotation
