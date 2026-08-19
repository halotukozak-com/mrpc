package halotukozak.mrpc.derive

import scala.concurrent.{ExecutionContext, Future}

import halotukozak.mcodec.MCodec

import halotukozak.mrpc.annotation.{multi, optional, rpcName, RpcTag}
import halotukozak.mrpc.conv.{AsRaw, AsReal}
import halotukozak.mrpc.raw.{RawRpc, RawRpcCompanion}

/**
 * Shared fixtures every derivation suite builds against. Declares a sample real trait exercising
 * each arity and shape the matcher and loopback must handle (a fire op, two call ops, a non-recursive
 * sub-RPC getter, an overloaded pair, a multi-param-list method, and a primitive/wrapper op), plus a
 * concrete `Raw = String` companion wiring the leaf codec givens and a threaded ExecutionContext.
 *
 * This file declares only traits, a DTO, and the companion — no derivation is invoked yet, so it
 * compiles before any macro exists. The concrete-companion placement matches what the abstract-Raw
 * summon proof established: the leaf codec givens are imported in the companion's scope.
 */
object SampleApi:

  // A small DTO with a derived codec, the non-primitive payload the call ops carry.
  final case class User(id: Int, name: String) derives MCodec

  // A REST-style tag hierarchy reusing the established annotation-tag pattern. Tags are carried for
  // API completeness; with a fixed three-method raw trait there is no tag-selection branch to drive.
  sealed trait RestTag extends RpcTag
  final class GET extends RestTag
  final class POST extends RestTag

  // A distinct, NON-recursive sub-RPC trait returned by the `users` getter. Self/mutual recursion is
  // intentionally out of scope here.
  trait UsersRpc:
    def count(): Future[Int]

  // Self-referential RPC: a getter returning the SAME trait — the headline self-recursion cycle the
  // lazy seam must break without infinite inline expansion.
  trait SelfRpc:
    def value(): Future[Int]
    def child: SelfRpc

  // Mutually-referential pair: Ping.toPong -> Pong, Pong.toPing -> Ping.
  trait Ping:
    def ping(): Future[Int]
    def toPong: Pong
  trait Pong:
    def pong(): Future[Int]
    def toPing: Ping

  // A fixed deeper SelfRpc level. `child` returns this leaf, so the runtime chain terminates (it does
  // NOT recurse forever) while still exercising one real nesting level through the recursion seam.
  private val leafSelf: SelfRpc = new SelfRpc:
    def value(): Future[Int] = Future.successful(99)
    def child: SelfRpc = this

  // A terminating self-referential impl: `child` hands back the fixed leaf, not an unbounded chain.
  val selfImpl: SelfRpc = new SelfRpc:
    def value(): Future[Int] = Future.successful(1)
    def child: SelfRpc = leafSelf

  // A terminating mutual pair: each side's getter returns the peer; the peers are fixed instances so
  // the runtime hop chain stays bounded.
  val pingImpl: Ping = new Ping:
    def ping(): Future[Int] = Future.successful(10)
    def toPong: Pong = pongImpl
  val pongImpl: Pong = new Pong:
    def pong(): Future[Int] = Future.successful(20)
    def toPing: Ping = pingImpl

  trait SampleApi:
    // fire op: empty-parens, Unit result -> routes to `fire`.
    def ping(): Unit

    // call ops: Future of a primitive and of the DTO -> route to `call`. The `@multi` marker on `n`
    // is additive — it carries a queryable per-param annotation for the metadata surface; the matcher
    // ignores it under the fixed-RawRpc model, so the derive suites stay green.
    def increment(@multi n: Int): Future[Int]
    def find(id: Int): Future[User]

    // sub-RPC getter -> routes to `get`; UsersRpc is a distinct trait, not self-referential.
    def users: UsersRpc

    // overloaded pair: same label, different signatures -> drives overload disambiguation.
    def lookup(id: Int): Future[User]
    def lookup(name: String): Future[User]

    // multi-param-list method -> drives the nested List[List[Raw]] <-> ParamLists mapping.
    def combine(a: Int)(b: String, c: Long): Future[String]

    // primitive/wrapper-arg op for decode-then-invoke dispatch safety.
    def echoBool(b: Boolean): Future[Boolean]

    // an explicitly renamed method, a positive target for name resolution.
    @rpcName("findOne") def findRenamed(id: Int): Future[User]

    // fire op with an Option-typed param (DIVERGENCES.md D7): None/Some round-trip through the
    // ordinary leaf codec (mcodec's Option codec writes null/the value) with no arity handling in
    // the dispatch path — @optional here is the same additive, matcher-ignored marker as @multi above.
    def configure(@optional timeout: Option[Int]): Unit

  // The concrete Raw = String companion the suites materialize against once the engine exists. The
  // leaf codec givens and a parasitic ExecutionContext (for the `call` arity's Future composition)
  // are brought into scope exactly where derivation will be invoked.
  object SampleApiCodec extends RawRpcCompanion[String]:
    import halotukozak.mrpc.codec.JsonRawValue.given

    given ExecutionContext = ExecutionContext.parasitic

    /**
     * Sanity references proving the leaf codec givens and the parasitic ExecutionContext resolve in
     * this companion's scope — the exact placement the derivation macros will rely on. Bound to
     * members so the import and the given are load-bearing before any macro is invoked.
     */
    val userEncoder: AsRawRpc[User] = summon[AsRawRpc[User]]
    val futureResultEncoder: AsRaw[Future[String], Future[User]] =
      summon[AsRaw[Future[String], Future[User]]]

    // Every `materializeAsRaw`/`materializeAsReal` call independently re-expands the full
    // RpcNames/OpPlan/AsRawDerivation macro cascade (inline defs are re-expanded at each call site,
    // not cached per type). Derived ONCE here and reused via import across every derivation suite,
    // instead of each suite re-deriving the same instances, to avoid paying that expansion N times.

    // Non-recursive sub-RPC conversions the `users`/`SampleApi` get-arity seams summon.
    given usersRaw: AsRaw[RawRpc[String], UsersRpc] = materializeAsRaw[UsersRpc]
    given usersReal: AsReal[RawRpc[String], UsersRpc] = materializeAsReal[UsersRpc]

    // Self-referential wiring (lazy val given read back through makeLazy — the FINDINGS Pitfall-1
    // placement): SelfRpc.child returns SelfRpc, so the nested summon inside materializeAsRaw/Real
    // must resolve to this already-declared lazy given instead of re-deriving forever.
    lazy val selfRaw: AsRaw[RawRpc[String], SelfRpc] = AsRaw.makeLazy(materializeAsRaw[SelfRpc])
    lazy val selfReal: AsReal[RawRpc[String], SelfRpc] = AsReal.makeLazy(materializeAsReal[SelfRpc])
    given AsRaw[RawRpc[String], SelfRpc] = selfRaw
    given AsReal[RawRpc[String], SelfRpc] = selfReal

    // Mutually-referential wiring (Ping.toPong -> Pong, Pong.toPing -> Ping), same lazy-given-readback
    // mechanism: each side's adapter/proxy resolves its peer to the lazy placeholder, not a re-derivation.
    lazy val pingRaw: AsRaw[RawRpc[String], Ping] = AsRaw.makeLazy(materializeAsRaw[Ping])
    lazy val pingReal: AsReal[RawRpc[String], Ping] = AsReal.makeLazy(materializeAsReal[Ping])
    lazy val pongRaw: AsRaw[RawRpc[String], Pong] = AsRaw.makeLazy(materializeAsRaw[Pong])
    lazy val pongReal: AsReal[RawRpc[String], Pong] = AsReal.makeLazy(materializeAsReal[Pong])
    given AsRaw[RawRpc[String], Ping] = pingRaw
    given AsReal[RawRpc[String], Ping] = pingReal
    given AsRaw[RawRpc[String], Pong] = pongRaw
    given AsReal[RawRpc[String], Pong] = pongReal

    // The headline server-adapter/client-proxy pair every loopback/dispatch/property suite drives.
    given sampleApiRaw: AsRaw[RawRpc[String], SampleApi] = materializeAsRaw[SampleApi]
    given sampleApiReal: AsReal[RawRpc[String], SampleApi] = materializeAsReal[SampleApi]
