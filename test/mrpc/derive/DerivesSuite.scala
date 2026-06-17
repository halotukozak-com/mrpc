package mrpc.derive

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import mcodec.MCodec

import mrpc.conv.{AsRaw, AsReal, RpcCodec}
import mrpc.raw.RawRpc

/**
 * End-to-end proof of the `derives RpcCodec` entry point — mrpc's analog of commons'
 * `object SomeApi extends RPCCompanion[SomeApi]`. There is NO manual `materializeAsRaw`/
 * `materializeAsReal` call and no hand-written `given`: the server adapter and client proxy are
 * summoned directly, because the trait opted in with `derives RpcCodec` and the gated auto-derivation
 * givens ([[AsRaw.derivedRpc]] / [[AsReal.derivedRpc]]) resolve at the concrete `Raw = String` site.
 */
object DerivesSuite:
  final case class User(id: Int, name: String) derives MCodec

  trait Calc derives RpcCodec:
    def ping(): Unit // fire
    def add(a: Int)(b: Int): Future[Int] // call, multiple param lists
    def find(id: Int): Future[User] // call, DTO result

class DerivesSuite extends munit.FunSuite:
  import DerivesSuite.*
  import mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  private def await[A](f: Future[A]): A = Await.result(f, Duration.Inf)

  private var pinged = false
  private val impl: Calc = new Calc:
    def ping(): Unit = pinged = true
    def add(a: Int)(b: Int): Future[Int] = Future.successful(a + b)
    def find(id: Int): Future[User] = Future.successful(User(id, s"u$id"))

  // The whole point: summon, do not materialize. `derives RpcCodec` makes these resolve automatically.
  private val server: RawRpc[String] = mrpc.showAst {
    AsRaw[RawRpc[String], Calc].asRaw(impl)
  }
  private val proxy: Calc = AsReal[RawRpc[String], Calc].asReal(server)

  test("derives RpcCodec: full real->raw->real round-trip with no manual materialize"):
    assertEquals(await(proxy.add(2)(3)), 5)
    assertEquals(await(proxy.find(7)), User(7, "u7"))
    pinged = false
    proxy.ping()
    assert(pinged)
