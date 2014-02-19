/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014. TAJ - Société d'avocats
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * EXCEPT AS CONTAINED IN THIS NOTICE, THE NAME OF TAJ - Société d'avocats SHALL
 * NOT BE USED IN ADVERTISING OR OTHERWISE TO PROMOTE THE SALE, USE OR OTHER
 * DEALINGS IN THIS SOFTWARE WITHOUT PRIOR WRITTEN AUTHORIZATION FROM
 * TAJ - Société d'avocats.
 */

package com.taj.unicode_detector.test

import org.scalatest._
import java.io.{RandomAccessFile, File}
import akka.actor.ActorSystem
import com.taj.unicode_detector._
import akka.testkit.{TestProbe, ImplicitSender, TestKit, TestActorRef}
import scala.concurrent.duration._

import com.taj.unicode_detector.AnalyzeFile
import com.taj.unicode_detector.FinalFullCheckResult
import com.taj.unicode_detector

/**
 * A case class to contain the parameters of a test file.
 * @param fileName the name of the test file.
 * @param encoding Encoding type of the file.
 */
case class testFileContainer(fileName: String, encoding: BOMFileEncoding, asciiContent: Boolean, workingActorsNeeded: Int)

/**
 * Test the detection algorithm with each kind of file.
 */
class Tester extends TestKit(ActorSystem("testSystem")) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"
  val testFilesFolder = testResourcesFolder + s"temp${File.separator}"


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
    new File(testFilesFolder).listFiles().foreach(_.delete())
  }

  override def beforeAll() {
    new File(testFilesFolder).listFiles().foreach(_.delete())
  }

  Seq(utf8_with_BOM, utf8_without_BOM, UTF16_BE, UTF16_LE, ASCII, utf8_with_BOM_bis, utf8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis)
    .foreach {
    fileToTest =>
      s"${fileToTest.fileName} file" must {
        "has a correct evaluation of workers quantity needed" in {
          bytesToRead = new File(encodedFileFolder, fileToTest.fileName).length()
          workerCount = (1 to Runtime.getRuntime.availableProcessors)
            .find(_ * ParamAkka.bufferSize >= bytesToRead)
            .getOrElse(Runtime.getRuntime.availableProcessors)
          //The block test
          workerCount should equal(fileToTest.workingActorsNeeded)
        }

        s"is detected as${if (!fileToTest.encoding.equals(BOM.ASCII)) " non" else ""} ASCII" in {
          val testActor = TestProbe()
          val worker = TestActorRef(new FileAnalyzer(testActor.ref, workerCount, bytesToRead))
          worker ! AnalyzeFile(encodedFileFolder + fileToTest.fileName)
          val resultToTest = testActor.receiveOne(40 seconds).asInstanceOf[FinalFullCheckResult]
          //The block test
          resultToTest.isASCII should equal(fileToTest.asciiContent)
        }

        s"is detected as ${fileToTest.encoding.name} based on its BOM" in {
          val detection = BOM.detect(encodedFileFolder + fileToTest.fileName)
          detection should equal(fileToTest.encoding)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_with_BOM_bis), (utf8_without_BOM, utf8_without_BOM_bis), (UTF16_BE, UTF16_BE_bis), (UTF16_LE, UTF16_LE_bis)).foreach {
    case (file1, file2) =>
      s"${file1.fileName} and ${file2.fileName}" must {
        "have the same detected BOM" in {
          val same = BOM.isSameBOM(true, file1.encoding, encodedFileFolder + file1.fileName, encodedFileFolder + file2.fileName)
          same should be(true)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_without_BOM), (UTF16_BE, utf8_with_BOM), (utf8_with_BOM, UTF16_LE), (UTF16_BE, UTF16_LE)).foreach {
    case (file1, file2) =>
      s"${file1.fileName} and ${file2.fileName}" must {
        "have different detected BOM" in {
          val same = BOM.isSameBOM(true, file1.encoding, encodedFileFolder + file1.fileName, encodedFileFolder + file2.fileName)
          same should be(false)
        }
      }
  }

  Seq(utf8_with_BOM, UTF16_BE, UTF16_LE, ASCII).foreach {
    file =>
      s"The BOM of the file ${file.fileName} will be removed and " must {
        val sourceFile = encodedFileFolder + file.fileName
        val destination = testFilesFolder + s"test_${file.encoding.name}.txt"
        val testFile = new File(destination)

        "the test file should be deleted before the test if it exists" in {
          if (testFile.exists()) {
            testFile.delete()
            Thread.sleep(100l)
            testFile should not be 'exists
          }
        }

        "the file must be detected as ASCII" in {
          BOM.copyWithoutBom(sourceFile, destination, true)
          BOM.detect(testFile.getAbsolutePath).name should be(unicode_detector.BOM.ASCII.name)
        }

        //tester la taille du fichier

        //tester le checksum des fichiers avec les manually cleaned

        //tester le contenu du fichier

        "the test file should be deleted after the test" in {
          val channel = new RandomAccessFile(testFile, "rw").getChannel()
          val lock = channel.tryLock()
          lock should not be null
          lock.release()
          channel.close()
          testFile should (be('delete) and not be 'exists)
        }
      }
  }
}


