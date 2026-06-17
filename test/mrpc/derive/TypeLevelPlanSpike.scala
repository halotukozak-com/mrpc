package mrpc.derive

import made.Done

import mrpc.derive.Plan.*
import mrpc.derive.SampleApi.*

/**
 * SPIKE finding (kept as documentation): arity classification is PARTIALLY type-level.
 *
 *   - Fire (`Unit`) and Call (`Future[r]`) reduce ENTIRELY at the type level via `Plan.ArityOf`
 *     (match-types over the `Done` mirror) — asserted below.
 *   - Get (sub-RPC) does NOT reduce. Distinguishing a sub-RPC result from a `Future` is undecidable
 *     for open result traits, and a `RpcInterface` marker supertype does not help: it only flips which
 *     side stalls (marker-first makes Call stall; Future-first makes Get stall). The split is inherently
 *     a "is the result itself an RPC?" question = given-resolution, so it stays in the macro (as commons).
 */
class TypeLevelPlanSpike extends munit.FunSuite:

  private type Head[T <: Tuple] = T match
    case h *: _ => h
  private type Tail[T <: Tuple] = T match
    case _ *: t => t
  private type At[T <: Tuple, N <: Int] = N match
    case 0 => Head[T]
    case _ => At[Tail[T], scala.compiletime.ops.int.-[N, 1]]

  private val done = summon[Done.Of[SampleApi]]
  private type Ops = done.Operations

  test("Fire/Call arity reduces fully at the type level (no macro)"):
    summon[ArityOf[At[Ops, 0]] =:= Fire] // ping(): Unit
    summon[ArityOf[At[Ops, 1]] =:= Call[Int]] // increment: Future[Int]
    summon[ArityOf[At[Ops, 2]] =:= Call[SampleApi.User]] // find: Future[User]
    summon[ArityOf[At[Ops, 6]] =:= Call[String]] // combine: Future[String]
    summon[ArityOf[At[Ops, 7]] =:= Call[Boolean]] // echoBool: Future[Boolean]
