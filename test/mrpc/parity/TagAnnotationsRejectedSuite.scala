package mrpc.parity

/**
 * `@methodTag`/`@paramTag`/`@tagged` used to compile and silently do nothing under mrpc's fixed
 * fire/call/get `RawRpc` (D9/D10). Now they're a compile error instead. Real tag-driven routing still
 * needs the generic raw-method framework.
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
