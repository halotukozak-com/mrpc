package halotukozak.mrpc.conv

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.parasitic
import scala.util.{Success, Try}

class AsRawSuite extends munit.FunSuite:
  test("identity asRaw returns its input"):
    assertEquals(AsRaw[Int, Int].asRaw(2), 2)

  test("forTry lifts an inner asRaw over Try without ambiguity"):
    val instance = summon[AsRaw[Try[Int], Try[Int]]]
    assertEquals(instance.asRaw(Success(2)), Success(2): Try[Int])

  test("forFuture lifts an inner asRaw over Future with an explicit ExecutionContext"):
    given scala.concurrent.ExecutionContext = parasitic
    // Distinct Raw/Real so forFuture (not identity) is selected and the EC is exercised.
    given AsRaw[String, Int] = (i: Int) => i.toString
    val instance = summon[AsRaw[Future[String], Future[Int]]]
    val result = instance.asRaw(Future.successful(2)).value.get.get
    assertEquals(result, "2")
