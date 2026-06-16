package mrpc.meta

/**
 * User-facing metadata entry point, mirroring `RawRpcCompanion`. A concrete companion mixes this in to
 * expose `materializeMetadata` alongside the engine's `materializeAsRaw`/`materializeAsReal`.
 */
trait RpcMetadataCompanion:
  inline def materializeMetadata[Real]: RpcMetadata[Real] =
    ${ mrpc.derive.MetadataDerivation.impl[Real] }
