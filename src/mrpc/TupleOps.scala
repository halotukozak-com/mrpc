package mrpc

extension (tup: Tuple)
  infix inline def realCons(x: Any): x.type *: tup.type =
    runtime.Tuples.cons(x, tup).asInstanceOf[x.type *: tup.type]
