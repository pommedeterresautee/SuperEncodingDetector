package com.taj.unicode_detector.test

import org.scalatest._
import java.io.File
import akka.actor.ActorSystem
import com.taj.unicode_detector._
import akka.testkit.{TestProbe, ImplicitSender, TestKit, TestActorRef}
import scala.concurrent.duration._

import com.taj.unicode_detector.AnalyzeFile
import com.taj.unicode_detector.FinalFullCheckResult

/**
 * A case class to contain the parameters of a test file.
 * @param fileName the name of the test file.
 * @param isASCII a boolean to say if it's a ASCII file.
 */
case class testFileContainer(fileName: String, isASCII: Boolean)

/**
 * Test the detection algorithm with each kind of file.
 */
class Tester extends TestKit(ActorSystem("testSystem")) with ImplicitSender with WordSpecLike with MustMatchers with BeforeAndAfterAll {
  val testFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}encoded_files${File.separator}"

  val utf8_with_BOM = testFileContainer("utf8_with_BOM.txt", isASCII = false)
  val utf8_without_BOM = testFileContainer("utf8_without_BOM.txt", isASCII = false)
  val UTF16_BE = testFileContainer("UTF16_BE.txt", isASCII = false)
  val UTF16_LE = testFileContainer("UTF16_LE.txt", isASCII = false)
  val ASCII = testFileContainer("ascii.txt", isASCII = true)

  var bytesToRead = 0L
  var workerCount = 0

  /**
   * Stops all actors when tests are finished.
   */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  Seq(utf8_with_BOM, utf8_without_BOM, UTF16_BE, UTF16_LE, ASCII)
    .foreach {
    fileToTest =>
      s"${fileToTest.fileName} file" must {
        "have a correct evaluation of workers quantity needed" in {
          println(utf8_with_BOM)
          bytesToRead = new File(testFolder, fileToTest.fileName).length()
          workerCount = (1 to Runtime.getRuntime.availableProcessors)
            .find(_ * ParamAkka.bufferSize >= bytesToRead)
            .getOrElse(Runtime.getRuntime.availableProcessors)
          //The block test
          workerCount must equal(1)
        }

        s"be detected as${if (!fileToTest.isASCII) " non" else ""} ASCII" in {
          val testActor = TestProbe()
          val worker = TestActorRef(new FileAnalyzer(testActor.ref, workerCount, bytesToRead))
          worker ! AnalyzeFile(testFolder + fileToTest.fileName)
          val resultToTest = testActor.receiveOne(40 seconds).asInstanceOf[FinalFullCheckResult]
          //The block test
          resultToTest.isASCII must be(fileToTest.isASCII)
        }
      }
  }
}



