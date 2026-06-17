package mrpc.meta

/**
 * The phantom base of every user metadata class, mirroring commons `metadata.scala:319`'s
 * `TypedMetadata[T]`.
 *
 * `T` is the type the metadata DESCRIBES — a method's result type or a parameter's type. It carries
 * no field; it exists so a metadata class can declare WHAT it is metadata-for, and so the materialize
 * macro / `@composite` recursion can summon nested metadata for the right `T`. A metadata class
 * extends `TypedMetadata[T]` and has its constructor params steered by the marker annotations
 * (`@reifyName`/`@reifyAnnot`/`@infer`/`@composite`/`@rpcMethodMetadata`/`@rpcParamMetadata`).
 */
trait TypedMetadata[T]
