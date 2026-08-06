package mrpc
package derive

sealed trait TypeProxy: // todo: erased
  type Underlying

  type Ev[_ >: Underlying <: Underlying]

  given Ev[Underlying] = compiletime.deferred

object TypeProxy:
  def apply[T, Ev0[_ >: T <: T]](using ev: Ev0[T]): TypeProxy { type Underlying = T; type Ev = Ev0 } =
    new TypeProxy:
      override type Underlying = T
      override type Ev = Ev0
      override given Ev[Underlying] = ev
