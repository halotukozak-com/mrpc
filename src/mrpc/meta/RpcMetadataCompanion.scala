package mrpc
package meta

import made.{Done, Made}
import mrpc.derive.RpcNames

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
 * names so metadata names cannot drift from the engine's.
 */
trait RpcMetadataCompanion[M[_]]:
  inline def materialize[Real: {Done.Of as done, RpcNames as names}](using made: Made.Of[M[Real]]): M[Real] =
    mrpc.derive.MetadataDerivation.impl[M, Real, names.Underlying](done.operations)
