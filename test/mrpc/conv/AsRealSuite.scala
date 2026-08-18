package halotukozak.mrpc.conv

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.parasitic
import scala.util.{Success, Try}

class AsRealSuite extends munit.FunSuite:
  test("identity asReal returns its input"):
    assertEquals(AsReal[Int, Int].asReal(2), 2)

  test("forTry lifts an inner asReal over Try without ambiguity"):
    val instance = summon[AsReal[Try[Int], Try[Int]]]
    assertEquals(instance.asReal(Success(2)), Success(2): Try[Int])

  test("forFuture lifts an inner asReal over Future with an explicit ExecutionContext"):
    given scala.concurrent.ExecutionContext = parasitic
    // Distinct Raw/Real so forFuture (not identity) is selected and the EC is exercised.
    given AsReal[String, Int] = (s: String) => s.toInt
    val instance = summon[AsReal[Future[String], Future[Int]]]
    val result = instance.asReal(Future.successful("2")).value.get.get
    assertEquals(result, 2)
