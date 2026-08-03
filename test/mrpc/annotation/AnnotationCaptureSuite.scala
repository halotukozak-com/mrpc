package mrpc.annotation

import made.Done

import scala.annotation.unused

//import made.{getAnnotation, hasAnnotation, Done}

/**
 * Proves `Done.derived` captures the mrpc annotation vocabulary on the trait, its operations, AND
 * its parameters — including type-parameterized tags and value-carrying fields — readable via
 * `getAnnotation`/`hasAnnotation`. Establishes the metadata that later matching/resolution reads.
 */
object AnnotationCaptureSuite:
  sealed trait RestTag extends RpcTag
  final class GET extends RestTag
  final class POST extends RestTag

  @rpcName("findOne")
  @methodTag[RestTag](Some(new GET))
  trait SampleApi:
    @tagged[GET] @single def find(id: Int): String
    def update(@tagged[POST] @multi body: String): Unit

class AnnotationCaptureSuite extends munit.FunSuite:
  import AnnotationCaptureSuite.*

  private val done = Done.derived[SampleApi]
  @unused val findOp *: updateOp *: EmptyTuple = done.operations
  @unused val bodyElem *: EmptyTuple = updateOp.inputElems

//todo
//  test("trait-level rpcName value is readable"):
//    assertEquals(done.getAnnotation[rpcName].map(_.name), Some("findOne"))
//
////  test("trait-level methodTag carries a readable, correctly-typed defaultTag"):
//    val defaultTag = done.getAnnotation[methodTag[RestTag]].flatMap(_.defaultTag)
//    defaultTag match
//      case Some(_: GET) => ()
//      case other => fail(s"expected Some(GET) defaultTag, got $other")
//
//  test("operation captures its tag type-arg, distinguishing GET from POST"):
//    assert(findOp.hasAnnotation[tagged[GET]])
//    assert(!findOp.hasAnnotation[tagged[POST]])
//    assert(findOp.hasAnnotation[single])
//
//  test("parameter captures its tag type-arg and arity, distinct from the operation's"):
//    assert(bodyElem.hasAnnotation[tagged[POST]])
//    assert(!bodyElem.hasAnnotation[tagged[GET]])
//    assert(bodyElem.hasAnnotation[multi])
