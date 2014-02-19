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
import java.io.{FileInputStream, RandomAccessFile, File}
import akka.actor.ActorSystem
import com.taj.unicode_detector._
import akka.testkit.{TestProbe, ImplicitSender, TestKit, TestActorRef}
import scala.concurrent.duration._

import com.taj.unicode_detector.AnalyzeFile
import com.taj.unicode_detector.FinalFullCheckResult
import com.taj.unicode_detector
import org.apache.commons.codec.digest.DigestUtils

/**
 * A case class to contain the parameters of a test file.
 * @param fileName the name of the test file.
 * @param encoding Encoding type of the file.
 */
case class testFileProperties(fileName: String, encoding: BOMFileEncoding, asciiContent: Boolean, workingActorsNeeded: Int)

/**
 * Test the detection algorithm with each kind of file.
 */
class Tester extends TestKit(ActorSystem("testSystem")) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"
  val tempFilesFolder = testResourcesFolder + s"temp${File.separator}"

  // First serie of text files with or without BOM
  val utf8_with_BOM = testFileProperties("utf8_with_BOM.txt", BOM.UTF8, asciiContent = false, 1)
  val utf8_without_BOM = testFileProperties("utf8_without_BOM.txt", BOM.UTF8NoBOM, asciiContent = false, 1)
  val UTF16_BE = testFileProperties("UTF16_BE.txt", BOM.UTF16BE, asciiContent = false, 1)
  val UTF16_LE = testFileProperties("UTF16_LE.txt", BOM.UTF16LE, asciiContent = false, 1)
  val ASCII = testFileProperties("ascii.txt", BOM.ASCII, asciiContent = true, 1)
  // Second serie of files with BOM for comparison purpose
  val utf8_with_BOM_bis = testFileProperties("utf8_with_BOM_bis.txt", BOM.UTF8, asciiContent = false, 1)
  val utf8_without_BOM_bis = testFileProperties("utf8_without_BOM_bis.txt", BOM.UTF8NoBOM, asciiContent = false, 1)
  val UTF16_BE_bis = testFileProperties("UTF16_BE_bis.txt", BOM.UTF16BE, asciiContent = false, 1)
  val UTF16_LE_bis = testFileProperties("UTF16_LE_bis.txt", BOM.UTF16LE, asciiContent = false, 1)
  // Files with BOM manually cleaned
  val utf8_with_BOM_manually_cleaned = testFileProperties("utf8_with_BOM_manually_cleaned.txt", BOM.ASCII, asciiContent = true, 1)
  val UTF16_BE_manually_cleaned = testFileProperties("UTF16_BE_manually_cleaned.txt", BOM.ASCII, asciiContent = true, 1)
  val UTF16_LE_manually_cleaned = testFileProperties("UTF16_LE_manually_cleaned.txt", BOM.ASCII, asciiContent = true, 1)

  var fileSize = 0l
  var workerCount = 0

  /**
   * Stops all actors when tests are finished.
   * Delete all temp files.
   */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  override def beforeAll() {
    new File(tempFilesFolder).listFiles().filter(!_.getName.contentEquals(".gitignore")).foreach(_.delete())
  }

  Seq(utf8_with_BOM, utf8_without_BOM, UTF16_BE, UTF16_LE, ASCII, utf8_with_BOM_bis, utf8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis, utf8_with_BOM_manually_cleaned, UTF16_BE_manually_cleaned, UTF16_LE_manually_cleaned)
    .foreach {
    fileToTest =>
      s"${fileToTest.fileName} file" must {
        "has a correct evaluation of workers quantity needed" in {
          fileSize = new File(encodedFileFolder, fileToTest.fileName).length()
          workerCount = ParamAKKA.numberOfWorkerRequired(fileSize)
          workerCount should equal(fileToTest.workingActorsNeeded)
        }

        s"is detected as${if (!fileToTest.encoding.equals(BOM.ASCII)) " non" else ""} ASCII" in {
          val testActor = TestProbe()
          val worker = TestActorRef(new FileAnalyzer(testActor.ref, fileSize))
          worker ! AnalyzeFile(encodedFileFolder + fileToTest.fileName, verbose = false)
          val resultToTest = testActor.receiveOne(40 seconds).asInstanceOf[FinalFullCheckResult]
          //The block test
          resultToTest.isASCII should equal(fileToTest.asciiContent)
        }

        s"is detected as ${fileToTest.encoding.charsetUsed} based on its BOM" in {
          val detection = BOM.detect(encodedFileFolder + fileToTest.fileName)
          detection should equal(fileToTest.encoding)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_with_BOM_bis), (utf8_without_BOM, utf8_without_BOM_bis), (UTF16_BE, UTF16_BE_bis), (UTF16_LE, UTF16_LE_bis)).foreach {
    case (first, second) =>
      val firstPath = encodedFileFolder + first.fileName
      val firstFile = new File(firstPath)
      val secondPath = encodedFileFolder + second.fileName
      val secondFile = new File(secondPath)
      val tempPath = s"${tempFilesFolder}merged_${first.fileName}_${second.fileName}"
      val tempFile = new File(tempPath)

      s"${first.fileName} and ${second.fileName}" must {
        "have the same detected BOM" in {
          val same = BOM.isSameBOM(true, first.encoding, firstPath, secondPath)
          same should be(true)
        }

        s"Merge files ${first.fileName} and ${second.fileName} together" in {
          BOM.mergeFilesWithoutBom(true, tempPath, firstPath, secondPath)
          tempFile should be('exists)
          tempFile.length() should equal(firstFile.length() + secondFile.length() - first.encoding.BOM.size)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_without_BOM), (UTF16_BE, utf8_with_BOM), (utf8_with_BOM, UTF16_LE), (UTF16_BE, UTF16_LE)).foreach {
    case (file1, file2) =>
      s"${file1.fileName} and ${file2.fileName}" must {
        "have different detected BOM" in {
          val same = BOM.isSameBOM(false, file1.encoding, encodedFileFolder + file1.fileName, encodedFileFolder + file2.fileName)
          same should be(false)
        }
      }
  }

  Seq((utf8_with_BOM, utf8_with_BOM_manually_cleaned), (UTF16_BE, UTF16_BE_manually_cleaned), (UTF16_LE, UTF16_LE_manually_cleaned), (ASCII, ASCII)).foreach {
    case (source, manuallyCleaned) =>
      s"The BOM of the file ${source.fileName} will be removed and " must {
        val sourcePath = encodedFileFolder + source.fileName
        val manuallyCleanedPath = encodedFileFolder + manuallyCleaned.fileName
        val tempFilePath = tempFilesFolder + s"temp_${source.encoding.charsetUsed}.txt"
        val sourceFile = new File(sourcePath)
        val tempFile = new File(tempFilePath)
        val manuallyCleanedFile = new File(manuallyCleanedPath)

        "the test file should be deleted before the test if it exists" in {
          if (tempFile.exists()) {
            tempFile.delete()
            tempFile should not be 'exists
          }
        }

        "the file must be detected as ASCII" in {
          BOM.copyWithoutBom(sourcePath, tempFilePath, verbose = true)
          tempFile should be('exists)
          BOM.detect(tempFile.getAbsolutePath).charsetUsed should be(unicode_detector.BOM.ASCII.charsetUsed)
        }

        s"the size of ${tempFile.getName} should be equal to the size of ${manuallyCleaned.fileName}" in {
          tempFile.length() should be(manuallyCleanedFile.length)
        }

        s"the size of ${tempFile.getName} should be equal to the size of ${source.fileName} minus the size of its BOM" in {
          sourceFile.length() should be > 0l
          tempFile should be('exists)
          tempFile.length() should be > 0l
          (sourceFile.length() - tempFile.length()) should equal(source.encoding.BOM.size.toLong)
        }

        s"the md5 of ${tempFile.getName} should be equal to the md5 of ${source.fileName}" in {
          tempFile should be('exists)
          val is1 = new FileInputStream(tempFile)
          val is2 = new FileInputStream(manuallyCleanedFile)
          DigestUtils.md5Hex(is1) should equal(DigestUtils.md5Hex(is2))
          is1.close()
          is2.close()
        }

        "the temp file should be deleted after the test" in {
          val channel = new RandomAccessFile(tempFile, "rw").getChannel()
          val lock = channel.tryLock()
          lock should not be null
          lock.release()
          channel.close()
          tempFile should (be('delete) and not be 'exists)
        }
      }
  }

  Seq(utf8_with_BOM, UTF16_BE, UTF16_LE, utf8_with_BOM_bis, utf8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis).foreach {
    file =>
      val fileToConvert = encodedFileFolder + file.fileName
      val convertedFile = tempFilesFolder + "converted_" + file.fileName
      s"convert the file ${file.fileName} to ASCII" in {
        Converter.convert2ASCII(fileToConvert, convertedFile, file.encoding, verbose = false)
        convertedFile.length should be > 0
      }
  }
}