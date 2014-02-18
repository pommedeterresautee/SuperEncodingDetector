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
 * @param encoding Encoding type of the file.
 */
case class testFileContainer(fileName: String, encoding: BOMFileEncoding, asciiContent: Boolean, workingActorsNeeded: Int)

/**
 * Test the detection algorithm with each kind of file.
 */
class Tester extends TestKit(ActorSystem("testSystem")) with ImplicitSender with WordSpecLike with MustMatchers with BeforeAndAfterAll {
  val testFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}encoded_files${File.separator}"

  val utf8_with_BOM = testFileContainer("utf8_with_BOM.txt", BOM.UTF8, asciiContent = false, 1)
  val utf8_without_BOM = testFileContainer("utf8_without_BOM.txt", BOM.ASCII, asciiContent = false, 1)
  val UTF16_BE = testFileContainer("UTF16_BE.txt", BOM.UTF16BE, asciiContent = false, 1)
  val UTF16_LE = testFileContainer("UTF16_LE.txt", BOM.UTF16LE, asciiContent = false, 1)
  val ASCII = testFileContainer("ascii.txt", BOM.ASCII, asciiContent = true, 1)
  val utf8_with_BOM_bis = testFileContainer("utf8_with_BOM_bis.txt", BOM.UTF8, asciiContent = false, 1)
  val utf8_without_BOM_bis = testFileContainer("utf8_without_BOM_bis.txt", BOM.ASCII, asciiContent = true, 1)
  val UTF16_BE_bis = testFileContainer("UTF16_BE_bis.txt", BOM.UTF16BE, asciiContent = false, 1)
  val UTF16_LE_bis = testFileContainer("UTF16_LE_bis.txt", BOM.UTF16LE, asciiContent = false, 1)

  var bytesToRead = 0L
  var workerCount = 0

  /**
   * Stops all actors when tests are finished.
   */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  Seq(utf8_with_BOM, utf8_without_BOM, UTF16_BE, UTF16_LE, ASCII, utf8_with_BOM_bis, utf8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis)
    .foreach {
    fileToTest =>
      s"${fileToTest.fileName} file" must {
        "have a correct evaluation of workers quantity needed" in {
          bytesToRead = new File(testFolder, fileToTest.fileName).length()
          workerCount = (1 to Runtime.getRuntime.availableProcessors)
            .find(_ * ParamAkka.bufferSize >= bytesToRead)
            .getOrElse(Runtime.getRuntime.availableProcessors)
          //The block test
          workerCount must equal(fileToTest.workingActorsNeeded)
        }

        s"be detected as${if (!fileToTest.encoding.equals(BOM.ASCII)) " non" else ""} ASCII" in {
          val testActor = TestProbe()
          val worker = TestActorRef(new FileAnalyzer(testActor.ref, workerCount, bytesToRead))
          worker ! AnalyzeFile(testFolder + fileToTest.fileName)
          val resultToTest = testActor.receiveOne(40 seconds).asInstanceOf[FinalFullCheckResult]
          //The block test
          resultToTest.isASCII must be(fileToTest.asciiContent)
        }

        s"be detected as ${fileToTest.encoding.name} based on its BOM" in {
          val detection = BOM.detect(testFolder + fileToTest.fileName)
          detection must equal(fileToTest.encoding)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_with_BOM_bis), (utf8_without_BOM, utf8_without_BOM_bis), (UTF16_BE, UTF16_BE_bis), (UTF16_LE, UTF16_LE_bis)).foreach{case (file1, file2) =>
    s"${file1.fileName} and ${file2.fileName}" must {
      "have the same detected BOM" in {
        val same = BOM.isSameBOM(true, file1.encoding, testFolder + file1.fileName, testFolder + file2.fileName)
        same must be(true)
      }
    }
  }

  Seq((utf8_with_BOM, utf8_without_BOM), (UTF16_BE, utf8_with_BOM), (utf8_with_BOM, UTF16_LE), (UTF16_BE, UTF16_LE)).foreach{case (file1, file2) =>
    s"${file1.fileName} and ${file2.fileName}" must {
      "have different detected BOM" in {
        val same = BOM.isSameBOM(true, file1.encoding, testFolder + file1.fileName, testFolder + file2.fileName)
        same must be(false)
      }
    }
  }
}


