package mrpc
package derive

sealed trait TypeProxy:
  type Underlying

object TypeProxy:

  private val reusable = new TypeProxy {}
  def apply[T]: TypeProxy { type Underlying = T } = reusable.asInstanceOf[TypeProxy { type Underlying = T }]
