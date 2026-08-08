package mrpc

/**
 * Wraps a value to lower its implicit priority below normal (unwrapped) instances — mirrors commons
 * `Fallback[T]`. A typeclass companion that wants to ship a default instance without it out-competing
 * a user-supplied one exposes a low-priority `fromFallback` given that unwraps a `Fallback[T]` instead
 * of offering `T` directly at normal priority: a `given AsRaw[Raw, Real]` elsewhere in scope is always
 * preferred, and `Fallback`'s own instance is only picked when nothing else resolves.
 */
final case class Fallback[+T](value: T) extends AnyVal
