package mrpc.meta

import scala.annotation.Annotation
import scala.compiletime.summonInline

import made.annotation.MetaAnnotation

/**
 * Shared runtime annotation-query helpers. The accessors are `inline` so each call site summons the
 * class evidence for the concrete annotation type, making the runtime filter checkable without a cast.
 * The class-evidence type is referenced fully-qualified to keep the import list free of any
 * macro-reflection vocabulary in this Done-first value layer.
 */
private object AnnotationQuery:
  inline def find[A <: MetaAnnotation](annotations: List[Annotation]): Option[A] =
    val tag = summonInline[scala.reflect.ClassTag[A]]
    annotations.collectFirst { case tag(a) => a }

/**
 * Materialized, runtime description of a real RPC trait's API surface, derived from `made.Done`.
 * Structural v1: name + operations + per-op params + annotation accessors. NOT the commons
 * TypedMetadata DSL (deferred to v2).
 *
 * The `Real` type parameter is a phantom that ties the materialized value to the trait it describes;
 * it carries no field.
 */
final case class RpcMetadata[Real](
  name: String,
  // @rpcMethodMetadata projection: one entry per RPC method (per-method metadata slot).
  operations: List[OperationMetadata],
  annotations: List[Annotation],
):
  inline def getAnnotation[A <: MetaAnnotation]: Option[A] = AnnotationQuery.find[A](annotations)
  inline def hasAnnotation[A <: MetaAnnotation]: Boolean = getAnnotation[A].isDefined

final case class OperationMetadata(
  name: String, // the RESOLVED rpcName (RpcName.computeAll) — the metadata identity
  label: String, // the raw method label (pre-resolution) — for parity diffing
  arity: String, // "fire" | "call" | "get" — same classification the matcher uses
  // @rpcParamMetadata projection: one entry per param, declaration order, flattened across lists.
  params: List[ParamMetadata],
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
