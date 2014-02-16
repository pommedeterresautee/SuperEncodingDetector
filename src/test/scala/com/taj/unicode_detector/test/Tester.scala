package com.taj.unicode_detector.test

import org.scalatest._
import java.io.File
import akka.actor.ActorSystem
import com.taj.unicode_detector._
import akka.testkit.{TestProbe, ImplicitSender, TestKit, TestActorRef}
import scala.concurrent.duration._

import akka.util.Timeout
import com.taj.unicode_detector.AnalyzeFile
import com.taj.unicode_detector.FinalFullCheckResult

class Tester extends TestKit(ActorSystem("testSystem"))
// Using the ImplicitSender trait will automatically set `testActor` as the sender
with ImplicitSender
with WordSpecLike
with MustMatchers with BeforeAndAfterAll {

  val pathToFEC = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\test.txt"
  val percentageToAnalyze = 100

  var bytesToRead = 0L
  var workerCount = 0

  override def afterAll(): Unit = {
    system.shutdown()
  }

  "ASCII file" must {
    "be correctly parameterized" in {
      bytesToRead = new File(pathToFEC).length() * percentageToAnalyze / 100
      workerCount = (1 to Runtime.getRuntime.availableProcessors).find(_ * ParamAkka.bufferSize >= bytesToRead).getOrElse(Runtime.getRuntime.availableProcessors)

      workerCount must equal(1)
    }

    "be detected as ASCII" in {

      implicit val t = Timeout(20 seconds)
      val testActor = TestProbe()

      val worker = TestActorRef(new FileAnalyzer(testActor.ref, workerCount, bytesToRead))

      worker ! AnalyzeFile(pathToFEC)
      val resultToTest = testActor.receiveOne(40 seconds).asInstanceOf[FinalFullCheckResult]
      resultToTest.isASCII must be(false)
    }

  }
}



