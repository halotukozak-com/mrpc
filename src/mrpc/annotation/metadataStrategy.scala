package mrpc.annotation

import made.annotation.MetaAnnotation

/**
 * Marks a metadata slot that collects per-RPC-method metadata. Mirrors commons `rpcMethodMetadata`.
 * Honored structurally as the operations-list (per-method) projection; the full metadata-strategy DSL
 * is deferred.
 */
final class rpcMethodMetadata extends MetaAnnotation

/**
 * Marks a metadata slot that collects per-RPC-parameter metadata. Mirrors commons `rpcParamMetadata`.
 * Honored structurally as the per-op params-list projection; the full metadata-strategy DSL is
 * deferred.
 */
final class rpcParamMetadata extends MetaAnnotation

/**
 * Marks a metadata slot that collects per-RPC-type-parameter metadata. Mirrors commons
 * `rpcTypeParamMetadata`. Declared now for the Plan 02+ macro vocabulary; the per-type-param
 * projection is a stretch goal per research.
 */
final class rpcTypeParamMetadata extends MetaAnnotation
