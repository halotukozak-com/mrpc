package mrpc.derive

import scala.quoted.*

import mrpc.meta.RpcMetadata

/**
 * Placeholder materializer for the structural `RpcMetadata` value layer. The real Done-first fold lands
 * later; for now every `materializeMetadata` call site aborts at expansion so the source tree compiles
 * while consumers cannot yet materialize.
 */
private[mrpc] object MetadataDerivation:
  def impl[Real: Type](using Quotes): Expr[RpcMetadata[Real]] =
    import quotes.reflect.*
    report.errorAndAbort("metadata materialization not implemented yet")
