package mrpc.meta

import scala.quoted.*

sealed trait HashCode[T <: Singleton]:
  type Value <: String

object HashCode:
  inline given [T <: Singleton] => HashCode[T] = ${ impl[T] }

  private def impl[T <: Singleton: Type](using Quotes): Expr[HashCode[T]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val ConstantType(constant) = tpe.runtimeChecked
    val hash = constant.value.hashCode.toHexString
    ConstantType(StringConstant(hash)).asType match
      case '[type hash <: String; hash] =>
        '{
          new HashCode[T]:
            type Value = hash
        }

sealed trait TypeName[T]:
  type Value <: String

object TypeName:
  inline given [T] => TypeName[T] = ${ impl[T] }

  private def impl[T: Type](using Quotes): Expr[TypeName[T]] =
    import quotes.reflect.*
    ConstantType(StringConstant(TypeRepr.of[T].show)).asType match
      case '[type hash <: String; hash] =>
        '{
          new TypeName[T]:
            type Value = hash
        }
