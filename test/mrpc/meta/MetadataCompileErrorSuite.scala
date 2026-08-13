package halotukozak.mrpc.meta

/**
 * Compile-time negative tests for the metadata DSL. Uses munit's `compileErrors`, which compiles a
 * source string and returns the diagnostics (empty when it compiles). The metadata classes under test
 * are declared INSIDE the compiled string so a deliberately-broken fixture (missing `@infer` given,
 * `@single` arity mismatch) does not break the whole project build.
 *
 * Assertions match a SUBSTRING of the diagnostic, not the exact compiler text.
 */
class MetadataCompileErrorSuite extends munit.FunSuite:

  test("the compile-error mechanism reports type errors"):
    assert(compileErrors("val x: Int = \"s\"").nonEmpty)

  test("@infer with no available given fails to compile with the @infer clue"):
    val errors = compileErrors(
      """
      import halotukozak.mrpc.annotation.infer
      import halotukozak.mrpc.meta.{RpcMetadataCompanion, TypedMetadata}
      import halotukozak.mrpc.derive.SampleApi.SampleApi

      final class NoSuchGiven[T]

      final case class BadMeta[T](
        @infer(clue = "needs a NoSuchGiven instance") tag: NoSuchGiven[T],
      ) extends TypedMetadata[T]
      object BadMeta extends RpcMetadataCompanion[BadMeta]

      val _ = BadMeta.materialize[SampleApi]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for the missing @infer given")
    assert(
      errors.contains("@infer") || errors.contains("NoSuchGiven"),
      s"error should mention @infer / the missing type; got: $errors",
    )

  test("@single @reifyAnnot on a symbol lacking the annotation fails to compile"):
    val errors = compileErrors(
      """
      import halotukozak.mrpc.annotation.{multi, reifyAnnot, reifyName, rpcMethodMetadata, single}
      import halotukozak.mrpc.meta.{RpcMetadataCompanion, TypedMetadata}
      import halotukozak.mrpc.derive.SampleApi.SampleApi

      // A made MetaAnnotation that NO SampleApi method carries.
      final class neverPresent extends halotukozak.made.annotation.MetaAnnotation

      final case class SingleAnnotMeta[T](
        @reifyName name: String,
        @single @reifyAnnot must: neverPresent,
      ) extends TypedMetadata[T]

      final case class TraitSingle[T](
        @rpcMethodMetadata @multi methods: List[SingleAnnotMeta[?]],
      )
      object TraitSingle extends RpcMetadataCompanion[TraitSingle]

      val _ = TraitSingle.materialize[SampleApi]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for the absent @single @reifyAnnot annotation")
    assert(
      errors.contains("reifyAnnot") || errors.contains("neverPresent") || errors.contains("single"),
      s"error should mention the missing annotation; got: $errors",
    )

  test("@single @rpcMethodMetadata over a many-op trait fails the arity count check"):
    val errors = compileErrors(
      """
      import halotukozak.mrpc.annotation.{reifyName, rpcMethodMetadata, single}
      import halotukozak.mrpc.meta.{RpcMetadataCompanion, TypedMetadata}
      import halotukozak.mrpc.derive.SampleApi.SampleApi

      final case class MethodOnly[T](
        @reifyName name: String,
      ) extends TypedMetadata[T]

      // @single requires exactly one matching op; SampleApi has 9 -> compile error.
      final case class TraitSingleMethod[T](
        @rpcMethodMetadata @single only: MethodOnly[?],
      )
      object TraitSingleMethod extends RpcMetadataCompanion[TraitSingleMethod]

      val _ = TraitSingleMethod.materialize[SampleApi]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for the >1 @single arity mismatch")
    assert(
      errors.contains("single") || errors.contains("exactly one") || errors.contains("rpcMethodMetadata"),
      s"error should name the arity mismatch; got: $errors",
    )
