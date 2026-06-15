package mrpc.conv

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.parasitic
import scala.concurrent.duration.DurationInt
import scala.util.{Success, Try}

class AsRawSuite extends munit.FunSuite:
  test("identity asRaw returns its input"):
    assertEquals(AsRaw[Int, Int].asRaw(2), 2)

  test("forTry lifts an inner asRaw over Try without ambiguity"):
    val instance = summon[AsRaw[Try[Int], Try[Int]]]
    assertEquals(instance.asRaw(Success(2)), Success(2): Try[Int])

  test("forFuture lifts an inner asRaw over Future with an explicit ExecutionContext"):
    given scala.concurrent.ExecutionContext = parasitic
    val instance = summon[AsRaw[Future[Int], Future[Int]]]
    val result = Await.result(instance.asRaw(Future.successful(2)), 1.second)
    assertEquals(result, 2)
