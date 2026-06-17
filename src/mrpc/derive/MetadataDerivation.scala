package mrpc.derive

import scala.annotation.Annotation
import scala.quoted.*

import mrpc.meta.{OperationMetadata, ParamMetadata, RpcMetadata}

/**
 * Metadata materializer. It is deliberately THIN: the only code it EMITS is the [[RpcMetadata]]
 * data-constructor call. Everything structural is REUSED from the engine's shared symbol-reflection
 * introspection — the SAME members and name resolution the matcher/dispatcher use — so the metadata
 * cannot drift from what the engine actually dispatches:
 *
 *   - op member symbols + their order come from [[Matcher.operationMembersOf]] (the engine's walk),
 *   - resolved rpcNames come from [[RpcName.computeAll]] (the SAME names the engine routes on),
 *   - the fire/call/get arity tag comes from [[Matcher.arityTagOf]] (the engine's classifier),
 *   - labels / params / per-param + per-method annotations come from [[OpReflect]].
 *
 * Contrast `AsRawDerivation`: this materializer synthesizes no new class symbol, calls no operation,
 * decodes no arguments, and performs no runtime cast. The only genuinely-new helper is
 * [[collectAnnotations]], which reifies the symbol's `MetaAnnotation` terms into runtime `Annotation`
 * instances (collecting ALL entries, not finding one).
 *
 * The metadata-strategy markers are honored STRUCTURALLY: the `operations` list is the per-method
 * projection and each op's `params` list is the per-param projection — there is NO per-annotation
 * steering of which slot collects what (the full strategy DSL is deferred). The split is fixed by the
 * value-type shape; recognition lives in `RpcMetadata.recognizedStrategies`.
 */
private[mrpc] object MetadataDerivation:

  def impl[Real: Type](using Quotes): Expr[RpcMetadata[Real]] =
    import quotes.reflect.*

    val members = Matcher.operationMembersOf[Real] // reuse: the engine's op member symbols, in order
    val names: List[String] = RpcName.computeAll[Real](members) // reuse: the SAME engine-dispatched names

    val traitName: String = TypeRepr.of[Real].typeSymbol.name

    val opExprs: List[Expr[OperationMetadata]] =
      members.zip(names).map { (member, name) =>
        val label = OpReflect.labelOf(member)
        val arity = Matcher.arityTagOf[Real](member) // reuse: the engine's arity classifier
        val paramExprs: List[Expr[ParamMetadata]] =
          OpReflect.params[Real](member).zip(OpReflect.paramAnnotations(member)).map { (p, anns) =>
            val pAnnots = collectAnnotations(anns)
            '{ ParamMetadata(${ Expr(p.label) }, $pAnnots) }
          }
        val opAnnots = collectAnnotations(OpReflect.methodAnnotations(member))
        '{
          OperationMetadata(
            ${ Expr(name) },
            ${ Expr(label) },
            ${ Expr(arity) },
            ${ Expr.ofList(paramExprs) },
            $opAnnots,
          )
        }
      }

    // Trait-level annotation fold is not required by the structural v1 surface (the must is per-op +
    // per-param). A future trait-level fixture can reuse `collectAnnotations` over a trait-level
    // metadata read here.
    val traitAnnots: Expr[List[Annotation]] = '{ List.empty[Annotation] }

    '{ RpcMetadata[Real](${ Expr(traitName) }, ${ Expr.ofList(opExprs) }, $traitAnnots) }

  /**
   * Reifies the member's/parameter's `MetaAnnotation` terms (read directly off the symbol) into
   * runtime `Annotation` instances. Each `MetaAnnotation` extends `StaticAnnotation` (`<: Annotation`),
   * so the annotation term ascribes directly; ALL entries are collected (the metadata surface exposes
   * every annotation for querying, not just the first match).
   */
  private def collectAnnotations(using q: Quotes)(anns: List[q.reflect.Term]): Expr[List[Annotation]] =
    Expr.ofList(anns.map(_.asExprOf[Annotation]))
