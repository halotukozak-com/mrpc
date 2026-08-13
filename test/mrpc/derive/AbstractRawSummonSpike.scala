package halotukozak.mrpc.derive

import scala.concurrent.{ExecutionContext, Future}

import mcodec.MCodec

import halotukozak.mrpc.conv.{AsRaw, AsReal}
import halotukozak.mrpc.raw.RawRpcCompanion

/**
 * Compile-only de-risking proof for the single biggest open question of the derivation engine:
 * whether a leaf `AsRaw`/`AsReal[Raw, t]` resolves when `Raw` is the abstract type parameter of a
 * raw-RPC companion AND the leaf givens live in the concrete companion's scope.
 *
 * If this file compiles, the question is answered: the derivation macros may REQUIRE such instances
 * abstractly (a generic context where `Raw` is a free type), and those requirements are satisfiable
 * at the concrete companion callsite where the leaf codec givens are imported. There is no test body
 * — successful compilation is the proof.
 */
object AbstractRawSummonSpike:

  // A small DTO whose codec is derived, exercising the codec-bridge path (not just primitives).
  final case class User(id: Int, name: String) derives MCodec

  // Level 1 — the abstract context. This mirrors what a derivation macro impl does: it REQUIRES an
  // `AsReal[Raw, A]` (and `AsRaw[Raw, A]`) for an abstract `Raw`, threaded as a using parameter.
  // That the body type-checks proves the engine can demand these instances generically.
  def proveAbstract[Raw, A](using AsReal[Raw, A], AsRaw[Raw, A]): Unit = ()

  // Level 2 — the concrete companion callsite. A transport instantiates the companion with a
  // concrete `Raw` (here `String`) and brings the leaf codec givens into scope. This is exactly the
  // placement the derivation macros will rely on: leaf givens imported alongside the companion, with
  // the threaded `ExecutionContext` the `call` arity needs for `forFuture` composition.
  object Concrete extends RawRpcCompanion[String]:
    import halotukozak.mrpc.codec.JsonRawValue.given

    given ExecutionContext = ExecutionContext.parasitic

    // Leaf primitive conversions resolve at the concrete `Raw = String` site.
    summon[AsReal[String, Int]]
    summon[AsRaw[String, Int]]

    // The codec-derived DTO conversion resolves too.
    summon[AsRaw[String, User]]
    summon[AsReal[String, User]]

    // The `forFuture` composition the `call` arity depends on resolves from the leaf instance plus
    // the in-scope parasitic ExecutionContext.
    summon[AsRaw[Future[String], Future[Int]]]
    summon[AsReal[Future[String], Future[Int]]]

    // The abstract requirement from Level 1 is satisfiable at this concrete site.
    proveAbstract[String, Int]
    proveAbstract[String, User]
