package halotukozak.mrpc.raw

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.mutable

class RawRpcSuite extends munit.FunSuite:

  given ExecutionContext = ExecutionContext.parasitic

  /** A tiny hand-written RawRpc[String] that dispatches by rpcName through fire/call/get. */
  final class StringRpc(fired: mutable.Buffer[RawInvocation[String]]) extends RawRpc[String]:

    def fire(invocation: RawInvocation[String]): Unit =
      fired += invocation

    def call(invocation: RawInvocation[String]): Future[String] =
      invocation.rpcName match
        case "add" =>
          val sum = invocation.args.flatten.map(_.toInt).sum
          Future.successful(sum.toString)
        case other =>
          Future.failed(new NoSuchElementException(s"unknown rpc: $other"))

    def get(invocation: RawInvocation[String]): RawRpc[String] =
      invocation.rpcName match
        case "sub" => StringRpc(fired)
        case other => throw new NoSuchElementException(s"unknown getter: $other")

  test("fire performs its side effect"):
    val fired = mutable.Buffer.empty[RawInvocation[String]]
    val rpc = StringRpc(fired)
    val inv = RawInvocation[String]("ping", List(List("7")))
    rpc.fire(inv)
    assertEquals(fired.toList, List(inv))

  test("call dispatches by rpcName and returns Future[Raw]"):
    val rpc = StringRpc(mutable.Buffer.empty)
    val result = rpc.call(RawInvocation[String]("add", List(List("2", "40")))).value.get.get
    assertEquals(result, "42")

  test("call fails for an unknown rpcName"):
    val rpc = StringRpc(mutable.Buffer.empty)
    intercept[NoSuchElementException]:
      rpc.call(RawInvocation[String]("nope", Nil)).value.get.get

  test("get returns a nested RawRpc that itself dispatches"):
    val rpc = StringRpc(mutable.Buffer.empty)
    val nested = rpc.get(RawInvocation[String]("sub", Nil))
    val result = nested.call(RawInvocation[String]("add", List(List("1", "2")))).value.get.get
    assertEquals(result, "3")
