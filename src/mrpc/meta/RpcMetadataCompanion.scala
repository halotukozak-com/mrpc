package mrpc.meta

/**
 * User-facing metadata entry point, mirroring commons `RpcMetadataCompanion`. A concrete metadata
 * class's COMPANION mixes this in, parameterized by the metadata type constructor `M[_]`, to expose
 * `materialize[Real]: M[Real]` — the metadata-class-param-driven derivation.
 *
 * Example (research §"Code Examples"):
 * {{{
 *   final case class TraitMeta[T](
 *     @reifyName name: String,
 *     @rpcMethodMetadata @multi methods: List[MethodMeta[?]],
 *   )
 *   object TraitMeta extends RpcMetadataCompanion[TraitMeta]
 *   // ...
 *   val md: TraitMeta[SampleApi] = TraitMeta.materialize[SampleApi]
 * }}}
 *
 * The macro reads `M`'s primary-constructor params, classifies each by its steering annotation, and
 * fills it from the real trait's `made.Done` structure — reusing `RpcName.computeAll` for resolved
 * names and `Matcher.arityTagOf` for arity so metadata names cannot drift from the engine's.
 */
trait RpcMetadataCompanion[M[_]]:
  inline def materialize[Real]: M[Real] =
    ${ mrpc.derive.MetadataDerivation.impl[M, Real] }

/**
 * The v1 flat-`RpcMetadata` entry point, retained so the v1 `MetadataSuite`/`MetadataStrategySuite`
 * keep COMPILING until Plan 03 migrates them to the new `TypedMetadata` DSL. The v1 macro
 * (`MetadataDerivationV1.impl`) is unchanged; only the canonical `RpcMetadataCompanion` name was
 * generalized to the metadata-class-parameterized form above.
 */
trait RpcMetadataCompanionV1:
  inline def materializeMetadata[Real]: RpcMetadata[Real] =
    ${ mrpc.derive.MetadataDerivationV1.impl[Real] }
