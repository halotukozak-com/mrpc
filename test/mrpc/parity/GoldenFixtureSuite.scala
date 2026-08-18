package halotukozak.mrpc.parity

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

import halotukozak.mcodec.MCodec

import halotukozak.mrpc.annotation.{multi, optional, rpcName}
import halotukozak.mrpc.raw.{RawInvocation, RawRpc}
import halotukozak.mrpc.raw.RawRpcCompanion

/**
 * VAL-01 golden-fixture parity: prove that the client proxy mrpc derives emits exactly the
 * `RawInvocation` the committed golden fixtures (the `fixtures` JSON files) describe — same `rpcName`
 * and the same nested-arg JSON that commons produces for the equivalent trait.
 *
 * The proof reuses the commons `RPCTest` pattern: a hand-written recording `RawRpc[String]` captures
 * every `RawInvocation[String]` the proxy hands it (fire/call/get all record). We materialize the
 * proxy over that recorder, invoke each method with the fixture's inputs, then assert the captured
 * `rpcName` + rendered `args` against the fixture read straight off disk (the fixture is the single
 * source of truth).
 *
 * Assertion mode (see DIVERGENCES.md D6): byte-for-byte everywhere. mcodec's `Json.write` matches the
 * golden data for the in-scope fixtures — primitives AND the object DTO — so no parse-normalize step
 * is used. The captured `args` (`List[List[String]]` of JSON fragments) re-render to the fixture's
 * `args` JSON array verbatim. The additive `metadata` field is ignored (DIVERGENCES.md D5).
 */
class GoldenFixtureSuite extends munit.FunSuite:

  import halotukozak.mrpc.codec.JsonRawValue.given
  given ExecutionContext = ExecutionContext.parasitic

  // The object payload `emit` carries. Two String fields named `k1`/`k2` so mcodec emits
  // `{"k1":"v1","k2":"v2"}` (declaration order, no whitespace) — matching `tagged_emit.json` exactly.
  final case class Fields(k1: String, k2: String) derives MCodec

  // A parity trait whose methods correspond 1:1 to the in-scope golden fixtures. Names/renames and
  // arities are chosen so the derived proxy emits the fixtures' rpcNames + arg shapes.
  trait ParityApi:
    // call_add: rpcName "add", args [[2,40]] — a single param list of two Ints, call arity.
    def add(a: Int, b: Int): Future[Int]

    // fire_ping: rpcName "ping", args [[7]] — one Int param, fire arity (Unit).
    def ping(n: Int): Unit

    // multi_broadcast: rpcName "broadcast", args [[["a","b","c"]]] — ONE param whose value is a
    // collection (the @multi marker is additive; the matcher treats it as one List[String] param).
    def broadcast(@multi msgs: List[String]): Unit

    // rpcname_renamed_status: rpcName "v2_status" (via @rpcName), args [[404]].
    @rpcName("v2_status") def status(code: Int): Unit

    // tagged_emit: rpcName "emit", args [["warn",{"k1":"v1","k2":"v2"}]] — String + object DTO.
    def emit(level: String, fields: Fields): Unit

    // optional_configure_none/some: rpcName "configure", args [[null]] / [[30]] — Option's own leaf
    // codec already round-trips None <-> null and Some(v) <-> v, so no arity handling is needed here.
    def configure(@optional x: Option[Int]): Unit

  // The concrete Raw = String companion the proxy is materialized against. The leaf codec givens and
  // the ExecutionContext the macro needs are already in this suite's scope (imported above).
  object ParityCodec extends RawRpcCompanion[String]

  // The commons RPCTest recorder: every fire/call/get appends the RawInvocation it receives.
  private val captured = scala.collection.mutable.ArrayBuffer.empty[RawInvocation[String]]
  private val recordingRaw: RawRpc[String] = new RawRpc[String]:
    def fire(inv: RawInvocation[String]): Unit = captured += inv
    def call(inv: RawInvocation[String]): Future[String] =
      captured += inv
      Future.successful("null")
    def get(inv: RawInvocation[String]): RawRpc[String] =
      captured += inv
      this

  private val proxy: ParityApi =
    ParityCodec.materializeAsReal[ParityApi].asReal(recordingRaw)

  /** Reads a committed golden fixture as its exact on-disk JSON string (the single source of truth). */
  private def fixture(name: String): String =
    val src = Source.fromFile(s"fixtures/$name.json")
    try src.mkString.trim
    finally src.close()

  /**
   * Renders a captured invocation's `rpcName` + `args` back into the fixtures' one-line JSON shape.
   * Each captured arg is ALREADY a JSON fragment (the leaf `AsRaw[String, A]` is `Json.write`), so the
   * nested `List[List[String]]` re-assembles to the fixture's `args` array byte-for-byte. The additive
   * `metadata` field is ignored (DIVERGENCES.md D5).
   */
  private def render(inv: RawInvocation[String]): String =
    val args = inv.args.map(_.mkString("[", ",", "]")).mkString("[", ",", "]")
    s"""{"rpcName":"${inv.rpcName}","args":$args}"""

  private def lastEmitted: RawInvocation[String] =
    assert(captured.nonEmpty, "the proxy emitted no invocation")
    captured.last

  override def beforeEach(context: BeforeEach): Unit = captured.clear()

  test("call_add: add(2, 40) emits the call_add fixture byte-for-byte"):
    proxy.add(2, 40)
    assertEquals(render(lastEmitted), fixture("call_add"))

  test("fire_ping: ping(7) emits the fire_ping fixture byte-for-byte"):
    proxy.ping(7)
    assertEquals(render(lastEmitted), fixture("fire_ping"))

  test("multi_broadcast: broadcast(List(a,b,c)) emits the multi_broadcast fixture byte-for-byte"):
    proxy.broadcast(List("a", "b", "c"))
    assertEquals(render(lastEmitted), fixture("multi_broadcast"))

  test("rpcname_renamed_status: status(404) emits v2_status byte-for-byte"):
    proxy.status(404)
    assertEquals(render(lastEmitted), fixture("rpcname_renamed_status"))

  // Object fixture: per DIVERGENCES.md D6 the verdict is byte-for-byte — mcodec emits the two-field
  // DTO as `{"k1":"v1","k2":"v2"}` (declaration order, no whitespace), matching the fixture exactly.
  test("tagged_emit: emit(warn, Fields(v1,v2)) emits the object fixture byte-for-byte"):
    proxy.emit("warn", Fields("v1", "v2"))
    assertEquals(render(lastEmitted), fixture("tagged_emit"))

  test("optional_configure_none: configure(None) emits null byte-for-byte"):
    proxy.configure(None)
    assertEquals(render(lastEmitted), fixture("optional_configure_none"))

  test("optional_configure_some: configure(Some(30)) emits the bare value byte-for-byte"):
    proxy.configure(Some(30))
    assertEquals(render(lastEmitted), fixture("optional_configure_some"))
