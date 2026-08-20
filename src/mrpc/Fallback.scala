package halotukozak.mrpc

/** Wraps a value to lower its implicit priority below normal givens. Mirrors commons `Fallback[T]`. */
final case class Fallback[+T](value: T) extends AnyVal
