package mrpc.meta

import scala.annotation.Annotation
import scala.compiletime.summonInline

import mrpc.annotation.{MetaAnnotation, rpcMethodMetadata, rpcParamMetadata}

/**
 * Shared runtime annotation-query helpers. The accessors are `inline` so each call site summons the
 * class evidence for the concrete annotation type, making the runtime filter checkable without a cast.
 * The class-evidence type is referenced fully-qualified to keep the import list free of any
 * macro-reflection vocabulary in this value layer.
 */
private object AnnotationQuery:
  inline def find[A <: MetaAnnotation](annotations: List[Annotation]): Option[A] =
    val tag = summonInline[scala.reflect.ClassTag[A]]
    annotations.collectFirst { case tag(a) => a }

/**
 * Materialized, runtime description of a real RPC trait's API surface, derived by direct compile-time
 * reflection on the trait (no external mirror).
 * Structural v1: name + operations + per-op params + annotation accessors. NOT the commons
 * TypedMetadata DSL (deferred to v2).
 *
 * The `Real` type parameter is a phantom that ties the materialized value to the trait it describes;
 * it carries no field.
 */
final case class RpcMetadata[Real](
  name: String,
  // The @rpcMethodMetadata marker declares this slot as the per-method projection: one entry per
  // RPC method. The annotation is the observable statement of intent (its presence is what the
  // recognition hook below names); the materializer emits this same operations list.
  @rpcMethodMetadata operations: List[OperationMetadata],
  annotations: List[Annotation],
):
  inline def getAnnotation[A <: MetaAnnotation]: Option[A] = AnnotationQuery.find[A](annotations)
  inline def hasAnnotation[A <: MetaAnnotation]: Boolean = getAnnotation[A].isDefined

/**
 * The metadata-strategy markers honored by materialization. `operations` is annotated
 * `@rpcMethodMetadata` (per-method projection) and each op's `params` is annotated `@rpcParamMetadata`
 * (per-param projection); this hook names both so a consumer can observe they are recognized — not
 * ignored. The full commons strategy DSL (`@composite`/`@reifyAnnot`/`@infer`/multi-collection) is
 * deferred to v2 — only this structural split is honored.
 */
object RpcMetadata:
  val recognizedStrategies: Set[String] = Set("rpcMethodMetadata", "rpcParamMetadata")

final case class OperationMetadata(
  name: String, // the RESOLVED rpcName (RpcName.computeAll) — the metadata identity
  label: String, // the raw method label (pre-resolution) — for parity diffing
  arity: String, // "fire" | "call" | "get" — same classification the matcher uses
  // The @rpcParamMetadata marker declares this slot as the per-param projection: one entry per param,
  // declaration order, flattened across param lists.
  @rpcParamMetadata params: List[ParamMetadata],
  annotations: List[Annotation],
):
  inline def getAnnotation[A <: MetaAnnotation]: Option[A] = AnnotationQuery.find[A](annotations)
  inline def hasAnnotation[A <: MetaAnnotation]: Boolean = getAnnotation[A].isDefined

final case class ParamMetadata(
  name: String,
  annotations: List[Annotation],
):
  inline def getAnnotation[A <: MetaAnnotation]: Option[A] = AnnotationQuery.find[A](annotations)
  inline def hasAnnotation[A <: MetaAnnotation]: Boolean = getAnnotation[A].isDefined
