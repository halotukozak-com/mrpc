package mrpc
package derive

import made.*

import scala.compiletime.ops.int.+
import scala.quoted.*
import scala.Tuple.Tail
import scala.annotation.tailrec

object RpcNames:
  transparent inline private def buildBases(
    operations: Tuple,
  ): Tuple = inline operations match
    case _: EmptyTuple => EmptyTuple
    case _: (head *: tail) =>
      inline compiletime.erasedValue[head & DoneOperation] match
        case op =>
          realCons(
            base[op.Metadata, op.Label],
            buildBases(operations.tail.asInstanceOf[tail]),
          )

  // todo: i hope it can be demacronize
  transparent inline private def base[M <: Tuple, Label <: String]: String = ${ baseImpl[M, Label] }

  private def baseImpl[M <: Tuple: Type, Label <: String: Type](using quotes: Quotes): Expr[String] =
    import quotes.reflect.*

    @tailrec
    def loop[Tup <: Tuple: Type](using Quotes): Expr[String] = Type.of[Tup] match
      case '[EmptyTuple] =>
        '{ compiletime.constValue[Label] }
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[mrpc.annotation.rpcName] =>
            annot.asExprOf[mrpc.annotation.rpcName] match
              case Expr(x) => Expr(x.name)
          case _ => loop[ts]

    val res = loop[M]

    res.asTerm.tpe.dealias.asType match
      case '[type base <: String; base] =>
        Expr(Type.valueOfConstant[base].get)

  extension (op: DoneOperation)
    transparent inline private def overloadedSuffix: String = ${ overloadedSuffixImpl[op.InputElems] }
    transparent inline private def getRpcName: String | Null = ${ getRpcNameImpl[op.Metadata] }
    transparent inline private def getOverloadedOnly: Boolean | Null = ${ getOverloadedOnlyImpl[op.Metadata] }
    transparent inline private def getPrefix: String | Null = ${ getPrefixImpl[op.Metadata] }

  private def getRpcNameImpl[M <: Tuple: Type](using quotes: Quotes): Expr[String | Null] =
    import quotes.reflect.*

    @tailrec
    def loop[Tup <: Tuple: Type](using Quotes): Expr[String | Null] = Type.of[Tup] match
      case '[EmptyTuple] =>
        '{ null }
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[mrpc.annotation.rpcName] =>
            annot.asExprOf[mrpc.annotation.rpcName] match
              case Expr(x) => Expr(x.name)
          case _ => loop[ts]

    loop[M]
  private def getOverloadedOnlyImpl[M <: Tuple: Type](using quotes: Quotes): Expr[Boolean | Null] =
    import quotes.reflect.*

    @tailrec
    def loop[Tup <: Tuple: Type](using Quotes): Expr[Boolean | Null] = Type.of[Tup] match
      case '[EmptyTuple] =>
        '{ null }
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[mrpc.annotation.rpcNamePrefix] =>
            annot.asExprOf[mrpc.annotation.rpcNamePrefix] match
              case Expr(x) => Expr(x.overloadedOnly)
          case _ => loop[ts]

    loop[M]
  private def getPrefixImpl[M <: Tuple: Type](using quotes: Quotes): Expr[String | Null] =
    import quotes.reflect.*

    @tailrec
    def loop[Tup <: Tuple: Type](using Quotes): Expr[String | Null] = Type.of[Tup] match
      case '[EmptyTuple] =>
        '{ null }
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[mrpc.annotation.rpcNamePrefix] =>
            annot.asExprOf[mrpc.annotation.rpcNamePrefix] match
              case Expr(x) => Expr(x.prefix)
          case _ => loop[ts]

    loop[M]

  /**
   */
  transparent inline private def buildResolveds[Names <: Tuple, Values <: Tuple](
    bases: Tuple,
    operations: Tuple,
  )(using
    inline ev1: bases.type containsOnly String,
    inline ev2: operations.type containsOnly DoneOperation,
    inline ev3: Names containsOnly String,
    inline ev4: Values containsOnly Int,
  ): Tuple =
    inline operations match // for some reason it cannot match on (operations, bases)
      case _: EmptyTuple =>
        inline bases match
          case _: EmptyTuple => EmptyTuple
      case _: (op *: tailOps) =>
        inline bases match
          case _: (base *: tailBases) =>
            realCons(
              widen(buildResolved[Names, Values, base & String](operations.head.asInstanceOf[op & DoneOperation])),
              buildResolveds[Names, Values](
                bases.tail.asInstanceOf[tailBases & Tail[bases.type]],
                operations.tail.asInstanceOf[tailOps & Tail[operations.type]],
              ),
            )

  transparent inline private def buildResolved[Names <: Tuple, Values <: Tuple, Base <: String](
    op: DoneOperation,
  ): String =
    inline val overloaded = compiletime.constValue[Overloaded[Names, Values, Base]]
    inline val prefixed = applyPrefix2[Base](op, overloaded)
    // Only overloaded members without their own @rpcName disambiguation get the signature suffix.
    inline val res = inline if overloaded && !op.hasAnnotation[mrpc.annotation.rpcName] then
      prefixed + op.overloadedSuffix
    else prefixed
    compiletime.requireConst(res)
    widen(res)

  private type FirstIndexOfAcc[Tup <: Tuple, T, N <: Int] = Tup match
    case T *: tail => N
    case _ *: tail => FirstIndexOfAcc[tail, T, N + 1]
    case EmptyTuple => Nothing

  private type FirstIndexOf[Tup <: Tuple, T] = FirstIndexOfAcc[Tup, T, 0]

  private type Overloaded[Names <: Tuple, Values <: Tuple, Name <: String] <: Boolean = FirstIndexOf[Names, Name] match
    case Nothing => false
    case _ => true

  transparent inline private def applyPrefix2[Base <: String](op: DoneOperation, overloaded: Boolean): String =
    inline val base = compiletime.constValue[Base]
    inline op.getOverloadedOnly match
      case _: Null => base
      case overloadedOnly: Boolean =>
        inline if !overloadedOnly || overloaded then
          inline op.getPrefix match
            case prefix: String => prefix + base
        else base

  transparent inline private def nameOf[T]: String = ${ nameOfImpl[T] }
  private def nameOfImpl[T: Type](using Quotes) = Expr(Type.show[T])

  transparent inline private def buildTypeNames(
    elems: Tuple,
  )(using inline ev: elems.type containsOnly InputElem,
  ): Tuple = inline elems match
    case _: EmptyTuple => EmptyTuple
    case _: (head *: tail) =>
      inline compiletime.erasedValue[head & InputElem] match
        case ie => nameOf[ie.Type] *: buildTypeNames(elems.tail.asInstanceOf[tail & Tuple.Tail[elems.type]])
  transparent inline private def buildRpcNames[T: Done.Of as done](
    bases: Tuple,
  ): Tuple =
    // todo: add duplicates
    buildResolveds[
      NoDuplicates[bases.type],
      Tuple.Map[NoDuplicates[bases.type], Occurrences0[bases.type]], // n * n time
    ](
      bases,
      done.operations,
    )(
      using containsOnly.refl,
      containsOnly.refl,
      containsOnly.refl,
      containsOnly.refl,
    )

  transparent inline def materialize[T: Done.Of as done]: Tuple =
    buildRpcNames(buildBases(done.operations))

  private def overloadedSuffixImpl[Elems <: Tuple: Type](using Quotes): Expr[String] =
    val sig = TupleTraverse
      .traverseTuple[Elems, InputElem]
      .map:
        case '[{ type Type = t }] => Type.show[t]
      .mkString("(", ",", ")")
    val hash = sig.hashCode & 0x7fffffff
    Expr(s"_${hash.toHexString}")

  private type Occurrences0[Tup <: Tuple] = [T] =>> Tuple.Size[Tuple.Filter[
    Tup,
    [X] =>> X match
      case T => true
      case _ => false,
  ]]

  private type NoDuplicatesAcc[Tup <: Tuple, Acc <: Tuple] <: Tuple = Tup match
    case EmptyTuple => Acc
    case h *: t =>
      Tuple.Contains[Acc, h] match
        case true => NoDuplicatesAcc[t, Acc]
        case false => NoDuplicatesAcc[t, Tuple.Concat[Acc, h *: EmptyTuple]]

  private type NoDuplicates[Tup <: Tuple] = NoDuplicatesAcc[Tup, EmptyTuple]
