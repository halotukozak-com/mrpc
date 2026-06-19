package mrpc.derive

import made.Done

import scala.compiletime.summonAll
import scala.concurrent.Future

/**
 * The per-op [[RpcName]] typeclass, summoned for every operation via `summonAll`, resolves each op's
 * name independently (seeing siblings via `OuterType` for overload disambiguation).
 *
 * NOTE: works for op signatures WITHOUT `MetaAnnotation`s. When an op carries one (e.g. `@multi`),
 * `summonAll` fails: the compiler drops the annotation when inferring the `derived[Op]` type param,
 * producing `RpcName[op-without-annot]` which doesn't match the required (invariant) annotated type.
 */
class RpcNamePerOpSuite extends munit.FunSuite:

  trait Simple:
    def alpha(): Unit
    def beta(x: Int): Future[Int]
    def gamma: Future[String]

  test("summonAll of per-op RpcName (annotation-free trait) resolves each name"):
    val done = summon[Done.Of[Simple]]
    val names: List[String] =
      summonAll[Tuple.Map[done.Operations, RpcName]].toList.map(_.asInstanceOf[RpcName[?]].name)
    assertEquals(names.toSet, Set("alpha", "beta", "gamma"))
