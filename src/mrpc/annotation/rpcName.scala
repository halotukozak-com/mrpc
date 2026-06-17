package mrpc.annotation


/** Real-side name override for an RPC method. Mirrors commons `rpcName(name)`. */
final class rpcName(val name: String) extends MetaAnnotation
