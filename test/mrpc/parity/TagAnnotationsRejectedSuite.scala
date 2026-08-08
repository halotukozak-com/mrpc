package mrpc.parity

/**
 * Locks in DIVERGENCES.md D10 (post-fix state): `@methodTag`/`@paramTag`/`@tagged` used to compile
 * and silently do nothing under mrpc's fixed fire/call/get `RawRpc` (D9 — there is no tag-selection
 * branch for them to steer, unlike commons' generic raw-method framework). That silent-no-op landmine
 * is now a compile error instead (`hasAnnotation[X[?]]` guards in `Plans.materialize` and
 * `OpPlan.materialize`/`ParamPlan.encodingOf`), so a reader can't mistake "this compiles"
 * for "this routes by tag". This suite proves the compile error fires for all three annotations, at
 * both the method and the param level, once the trait is actually materialized (the guard runs during
 * `Plans.materialize`, reached from `materializeAsRaw`/`materializeAsReal`).
 *
 * The underlying gap — real tag-driven routing between several raw method/param variants — is still
 * not implemented; that requires the generic raw-method framework of D9 first.
 */
class TagAnnotationsRejectedSuite extends munit.FunSuite:

  test("@methodTag on a real trait fails to compile (DIVERGENCES.md D10)"):
    val errors = compileErrors(
      """
      import scala.concurrent.{ExecutionContext, Future}
      import mrpc.annotation.{methodTag, RpcTag}
      import mrpc.raw.RawRpcCompanion

      sealed trait RestTag extends RpcTag
      final class GET extends RestTag

      @methodTag[RestTag](Some(new GET))
      trait Api:
        def find(id: Int): Future[Int]

      object ApiCodec extends RawRpcCompanion[String]

      given ExecutionContext = ExecutionContext.parasitic
      val _ = ApiCodec.materializeAsReal[Api]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for @methodTag")
    assert(errors.contains("@methodTag"), s"error should name @methodTag; got: $errors")

  test("@paramTag on a real trait fails to compile (DIVERGENCES.md D10)"):
    val errors = compileErrors(
      """
      import scala.concurrent.{ExecutionContext, Future}
      import mrpc.annotation.{paramTag, RpcTag}
      import mrpc.raw.RawRpcCompanion

      sealed trait HeaderTag extends RpcTag
      final class Header extends HeaderTag

      @paramTag[HeaderTag](Some(new Header))
      trait Api:
        def find(id: Int): Future[Int]

      object ApiCodec extends RawRpcCompanion[String]

      given ExecutionContext = ExecutionContext.parasitic
      val _ = ApiCodec.materializeAsReal[Api]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for @paramTag")
    assert(errors.contains("@paramTag"), s"error should name @paramTag; got: $errors")

  test("@tagged on a real method fails to compile (DIVERGENCES.md D10)"):
    val errors = compileErrors(
      """
      import scala.concurrent.{ExecutionContext, Future}
      import mrpc.annotation.{tagged, RpcTag}
      import mrpc.raw.RawRpcCompanion

      sealed trait RestTag extends RpcTag
      final class GET extends RestTag

      trait Api:
        @tagged[GET] def find(id: Int): Future[Int]

      object ApiCodec extends RawRpcCompanion[String]

      given ExecutionContext = ExecutionContext.parasitic
      val _ = ApiCodec.materializeAsReal[Api]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for @tagged")
    assert(errors.contains("@tagged"), s"error should name @tagged; got: $errors")

  test("@tagged on a real param fails to compile (DIVERGENCES.md D10)"):
    val errors = compileErrors(
      """
      import scala.concurrent.ExecutionContext
      import mrpc.annotation.{tagged, RpcTag}
      import mrpc.raw.RawRpcCompanion

      sealed trait RestTag extends RpcTag
      final class GET extends RestTag

      trait Api:
        def update(@tagged[GET] body: String): Unit

      object ApiCodec extends RawRpcCompanion[String]

      given ExecutionContext = ExecutionContext.parasitic
      val _ = ApiCodec.materializeAsReal[Api]
      """,
    )
    assert(errors.nonEmpty, "expected a compile error for @tagged on a param")
    assert(errors.contains("@tagged"), s"error should name @tagged; got: $errors")
