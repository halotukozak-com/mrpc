package mrpc.derive

import scala.annotation.Annotation
import scala.quoted.*

import mrpc.meta.{OperationMetadata, ParamMetadata, RpcMetadata}

/**
 * Done-first metadata materializer. It is deliberately THIN: the only code it EMITS is the
 * [[RpcMetadata]] data-constructor call. Everything structural is REUSED from the engine's shared
 * introspection — the SAME Done-walk and name resolution the matcher/dispatcher use — so the metadata
 * cannot drift from what the engine actually dispatches:
 *
 *   - op types + their order come from [[Matcher.operationTypesOf]] (the engine's op-type walk),
 *   - resolved rpcNames come from [[RpcName.computeAll]] (the SAME names the engine routes on),
 *   - the fire/call/get arity tag comes from [[Matcher.arityTagOf]] (the engine's classifier),
 *   - labels / params / per-param + per-method annotation entries come from [[OpReflect]].
 *
 * Contrast `AsRawDerivation`: this materializer synthesizes no new class symbol, calls no operation,
 * decodes no arguments, and performs no runtime cast. The only genuinely-new helper is
 * [[collectAnnotations]], mirroring made's `getAnnotationImpl` but collecting ALL captured
 * `AnnotatedType(Meta, annot)` entries into runtime `Annotation` instances instead of finding one.
 *
 * The metadata-strategy markers are honored STRUCTURALLY: the `operations` list is the per-method
 * projection and each op's `params` list is the per-param projection — there is NO per-annotation
 * steering of which slot collects what (the full strategy DSL is deferred). The split is fixed by the
 * value-type shape; recognition lives in `RpcMetadata.recognizedStrategies`.
 */
private[mrpc] object MetadataDerivation:

  def impl[Real: Type](using Quotes): Expr[RpcMetadata[Real]] =
    import quotes.reflect.*

    val ops: List[Type[?]] = Matcher.operationTypes[Real](Matcher.summonDone[Real]) // refined op types, Done order
    val names: List[String] = RpcName.computeAll(ops) // reuse: the SAME engine-dispatched names

    val traitName: String = TypeRepr.of[Real].typeSymbol.name

    val opExprs: List[Expr[OperationMetadata]] =
      ops.zip(names).map { (opTpe, name) =>
        val label = OpReflect.labelOf(opTpe)
        val arity = Matcher.arityTagOf(opTpe) // reuse: the engine's arity classifier
        val paramExprs: List[Expr[ParamMetadata]] =
          OpReflect.inputElems(opTpe).map { p =>
            val pAnnots = collectAnnotations(p.metadataEntries)
            '{ ParamMetadata(${ Expr(p.label) }, $pAnnots) }
          }
        val opAnnots = collectAnnotations(OpReflect.metadataEntries(opTpe))
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
   * Reifies the captured `AnnotatedType(Meta, annot)` metadata entries into runtime `Annotation`
   * instances. Mirrors made's `getAnnotationImpl` extraction (`annot.asExprOf`) but collects ALL
   * entries rather than the first match — the metadata surface exposes every annotation for querying.
   */
  private def collectAnnotations(entries: List[Type[?]])(using q: Quotes): Expr[List[Annotation]] =
    import q.reflect.*
    val terms: List[Expr[Annotation]] = entries
      .map(t => TypeRepr.of(using t))
      .collect { case AnnotatedType(_, annot) => annot.asExprOf[Annotation] }
    Expr.ofList(terms)
