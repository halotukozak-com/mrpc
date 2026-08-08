package mrpc
package derive

import made.*
import made.annotation.MetaAnnotation

import scala.compiletime.ops.int.{+, >}
import scala.quoted.*
import scala.Tuple.Tail
import scala.annotation.tailrec
import mrpc.annotation.*

object RpcNames:
  transparent inline private def buildBases(
    operations: Tuple,
  )(using operations.type containsOnly DoneOperation,
  ): Tuple = inline operations match
    case _: EmptyTuple => EmptyTuple
    case _: (head *: tail) =>
      val head = operations.head.asInstanceOf[head & DoneOperation]
      realCons(head.base, buildBases(operations.tail.asInstanceOf[tail & Tail[operations.type]]))

  extension (op: DoneOperation)
    transparent inline private def base: String =
      inline op.getRpcName match
        case null => compiletime.constValue[op.Label]
        case rpcName => rpcName
    transparent inline private def overloadedSuffix: String = ${ overloadedSuffixImpl[op.InputElems] }
    transparent inline private def getRpcName: String | Null = ${ getRpcNameImpl[op.Metadata] }
    transparent inline private def getOverloadedOnly: Boolean | Null = ${ getOverloadedOnlyImpl[op.Metadata] }
    transparent inline private def getPrefix: String | Null = ${ getPrefixImpl[op.Metadata] }

  given [T: ToExpr] => ToExpr[T | Null]:
    override def apply(x: T | Null)(using Quotes): Expr[T | Null] =
      x match
        case null => '{ null }
        case x: T @unchecked => Expr(x)

  @tailrec
  private def loop[Tup <: Tuple: Type, Annot <: MetaAnnotation: {Type, FromExpr}](using quotes: Quotes): Option[Annot] =
    import quotes.reflect.*
    Type.of[Tup] match
      case '[EmptyTuple] =>
        None
      case '[t *: ts] =>
        TypeRepr.of[t] match
          case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[Annot] =>
            annot.asExprOf[Annot] match
              case Expr(x) => Some(x)
          case _ => loop[ts, Annot]

  private def getRpcNameImpl[M <: Tuple: Type](using Quotes): Expr[String | Null] =
    Expr(loop[M, mrpc.annotation.rpcName].map(_.name).orNull)

  private def getOverloadedOnlyImpl[M <: Tuple: Type](using Quotes): Expr[Boolean | Null] =
    Expr(loop[M, mrpc.annotation.rpcNamePrefix].map(_.overloadedOnly).orNull)

  private def getPrefixImpl[M <: Tuple: Type](using Quotes): Expr[String | Null] =
    Expr(loop[M, mrpc.annotation.rpcNamePrefix].map(_.prefix).orNull)

  /**
   */
  transparent inline private def buildResolveds[Names <: Tuple, Values <: Tuple](
    bases: Tuple,
    operations: Tuple,
  )(using
    bases.type containsOnly String,
    operations.type containsOnly DoneOperation,
    Names containsOnly String,
    Values containsOnly Int,
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

  private type FirstIndexOfAcc[Tup <: Tuple, T, N <: Int] <: Int = Tup match
    case T *: tail => N
    case _ *: tail => FirstIndexOfAcc[tail, T, N + 1]
    case EmptyTuple => Nothing

  private type FirstIndexOf[Tup <: Tuple, T] = FirstIndexOfAcc[Tup, T, 0]

  private type Overloaded[Names <: Tuple, Values <: Tuple, Name <: String] <: Boolean = FirstIndexOf[Names, Name] match
    case Nothing => false
    case ([n] =>> n)[n] => (Tuple.Elem[Values, n] & Int) > 1

  transparent inline private def applyPrefix2[Base <: String](op: DoneOperation, overloaded: Boolean): String =
    inline val base = compiletime.constValue[Base]
    inline op.getOverloadedOnly match
      case _: Null => base
      case overloadedOnly: Boolean =>
        inline if !overloadedOnly || overloaded then
          inline op.getPrefix match
            case prefix: String => prefix + base
        else base

  transparent inline private def buildRpcNames[T: Done.Of as done](
    bases: Tuple,
  )(using bases.type containsOnly String,
  ): Tuple = checkDuplicates(
    buildResolveds[
      NoDuplicates[bases.type],
      Tuple.Map[NoDuplicates[bases.type], Occurrences[bases.type]], // n * n time
    ](
      bases,
      done.operations,
    )(using summon, summon, containsOnly.refl, containsOnly.refl),
  )

  inline private def checkDuplicates(result: Tuple): result.type =
    inline if result.hasDuplicates then
      compiletime.error("Duplicate RPC names found. Consider using @rpcName to disambiguate.")
    result

  transparent inline def materialize[T: Done.Of as done]: Tuple =
    buildRpcNames(buildBases(done.operations))(using done, containsOnly.refl)

  private def overloadedSuffixImpl[Elems <: Tuple: Type](using Quotes): Expr[String] =
    val sig = TupleTraverse
      .traverseTuple[Elems, InputElem]
      .map:
        case '[{ type Type = t }] => Type.show[t]
      .mkString("(", ",", ")")
    val hash = sig.hashCode & 0x7fffffff
    Expr(s"_${hash.toHexString}")

  private type Occurrences0[Acc <: Tuple, T, N <: Int] = Acc match
    case EmptyTuple => N
    case T *: tail => Occurrences0[tail, T, N + 1]
    case _ *: tail => Occurrences0[tail, T, N]

  private type Occurrences[Tup <: Tuple] = [T] =>> Occurrences0[Tup, T, 0]

  private type NoDuplicates[Tup <: Tuple] <: Tuple = Tup match
    case EmptyTuple => EmptyTuple
    case h *: t =>
      Tuple.Contains[t, h] match
        case true => NoDuplicates[t]
        case false => h *: NoDuplicates[t]
