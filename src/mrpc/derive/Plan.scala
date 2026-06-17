package mrpc.derive

import scala.concurrent.Future

import made.DoneOperation

/**
 * Type-level plan vocabulary — the classification layer (A) expressed as TYPES and match-types over
 * `made.DoneOperation`, rather than as macro-computed value objects ([[OpPlan]]/[[Arity]] enums).
 *
 * The point: the matcher's per-operation classification (arity, encoding, …) becomes type-level
 * reduction over the `Done` mirror's own type members (`OutputType`, `InputElems`, `Metadata`), so the
 * macro shrinks to reading an already-computed type instead of imperatively walking reflection.
 *
 * `made.Done` already exposes the operation as a refined type, so these match-types reduce for any
 * concrete `Op` element of a `Done.Operations` tuple.
 */
object Plan:

  // --- type-level arity ---

  sealed trait Arity
  sealed trait Fire extends Arity
  sealed trait Call[R] extends Arity
  sealed trait Get[Sub] extends Arity

  /** The operation's `OutputType`, recovered structurally so it reduces for any concrete `Op`. */
  type OutputOf[Op <: DoneOperation] = Op match
    case HasOutput[o] => o
  type HasOutput[O] = DoneOperation { type OutputType = O }

  /**
   * Arity from the output type: `Unit` -> fire, `Future[X]` -> call carrying `X`, anything else -> a
   * sub-RPC getter seam carrying the sub type. Mirrors `Matcher.arityOf`, but as a match-type.
   */
  // Fire and Call reduce decidably (positive matches on Unit / Future). Get does NOT reduce here:
  // distinguishing a sub-RPC result from a Future is undecidable for open traits (a marker supertype
  // does not help — it only flips which side stalls). The Call-vs-Get split therefore stays in the
  // macro (given-resolution), exactly as commons does. `ArityOf` is thus a partial classifier.
  type ArityOf[Op <: DoneOperation] = OutputOf[Op] match
    case Unit => Fire
    case Future[r] => Call[r]
