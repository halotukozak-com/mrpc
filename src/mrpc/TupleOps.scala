package mrpc

inline def realCons(x: Any, tup: Tuple): x.type *: tup.type =
  runtime.Tuples.cons(x, tup).asInstanceOf[x.type *: tup.type]
