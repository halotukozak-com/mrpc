package mrpc.annotation

import made.annotation.MetaAnnotation

/** Value passes through the codec (the default). Mirrors commons `encoded`. */
final class encoded extends MetaAnnotation

/** Value IS the raw type, bypassing the codec. Mirrors commons `verbatim`. */
final class verbatim extends MetaAnnotation
