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
import akka.testkit.{ImplicitSender, TestKit}

import org.apache.commons.codec.digest.DigestUtils
import java.nio.charset.{StandardCharsets, Charset}

/**
 * A case class to contain the parameters of a test file.
 * @param fileName the name of the test file.
 * @param encoding Encoding type of the file.
 */
case class testFileProperties(fileName: String, encoding: BOMFileEncoding, workingActorsNeeded: Int)

/**
 * Test the detection algorithm with each kind of file.
 */
class Tester extends TestKit(ActorSystem("testSystem")) with ImplicitSender with WordSpecLike with Matchers with BeforeAndAfterAll {
  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"
  val tempFilesFolder = testResourcesFolder + s"temp${File.separator}"

  // First list of text files with or without BOM
  val UTF8_with_BOM = testFileProperties("UTF8_with_BOM.txt", BOMEncoding.UTF8, 1)
  val UTF8_without_BOM = testFileProperties("UTF8_without_BOM.txt", BOMEncoding.UTF8NoBOM, 1)
  val UTF16_BE = testFileProperties("UTF16_BE.txt", BOMEncoding.UTF_16_BE, 1)
  val UTF16_LE = testFileProperties("UTF16_LE.txt", BOMEncoding.UTF_16_LE, 1)
  val ASCII = testFileProperties("ASCII.txt", BOMEncoding.ASCII, 1)
  val Windows_1252 = testFileProperties("Windows_1252.txt", BOMFileEncoding(Charset.forName("ISO-8859-2"), List(), List()), 1)

  // Second list of files with BOM for comparison purpose
  val UTF8_with_BOM_bis = testFileProperties("UTF8_with_BOM_bis.txt", BOMEncoding.UTF8, 1)
  val UTF8_without_BOM_bis = testFileProperties("UTF8_without_BOM_bis.txt", BOMEncoding.UTF8NoBOM, 1)
  val UTF16_BE_bis = testFileProperties("UTF16_BE_bis.txt", BOMEncoding.UTF_16_BE, 1)
  val UTF16_LE_bis = testFileProperties("UTF16_LE_bis.txt", BOMEncoding.UTF_16_LE, 1)
  // Files with BOM manually cleaned
  val UTF8_with_BOM_manually_cleaned = testFileProperties("UTF8_with_BOM_manually_cleaned.txt", BOMEncoding.ASCII, 1)
  val UTF16_BE_manually_cleaned = testFileProperties("UTF16_BE_manually_cleaned.txt", BOMEncoding.ASCII, 1)
  val UTF16_LE_manually_cleaned = testFileProperties("UTF16_LE_manually_cleaned.txt", BOMEncoding.ASCII, 1)

  // Third list of text files with or without BOM and bad parameters
  val UTF8_with_BOM_error = testFileProperties("UTF8_with_BOM.txt", BOMEncoding.UTF_16_BE, 2)
  val UTF8_without_BOM_error = testFileProperties("UTF8_without_BOM.txt", BOMEncoding.UTF32LEUnusual, 2)
  val UTF16_BE_error = testFileProperties("UTF16_BE.txt", BOMEncoding.UTF_16_LE, 2)
  val UTF16_LE_error = testFileProperties("UTF16_LE.txt", BOMEncoding.UTF8, 2)
  val ASCII_error = testFileProperties("ASCII.txt", BOMEncoding.UTF8NoBOM, 2)
  val Windows_1252_error = testFileProperties("Windows_1252.txt", BOMEncoding.UTF_16_BE, 2)

  /**
   * Clean all temp files before starting
   */
  override def beforeAll() {
    val tmpFolder = new File(tempFilesFolder)
    tmpFolder.listFiles().filter(!_.getName.contentEquals(".gitignore")).foreach(_.delete())
    tmpFolder.listFiles().length should be > 0
  }

  /**
   * Stops all actors when tests are finished.
   * Delete all temp files.
   */
  override def afterAll(): Unit = {
    system.shutdown()
  }

  Seq(UTF8_with_BOM, UTF8_without_BOM, UTF16_BE, UTF16_LE, ASCII, Windows_1252, UTF8_with_BOM_bis, UTF8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis, UTF8_with_BOM_manually_cleaned, UTF16_BE_manually_cleaned, UTF16_LE_manually_cleaned)
    .foreach {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)
      var fileSize = 0l
      var workerCount = 0

      s"${fileToTest.fileName} file" must {
        s"Workers quantity should be evaluated equals to ${fileToTest.workingActorsNeeded}" in {
          fileSize = file.length()
          workerCount = ParamAkka.numberOfWorkerRequired(fileSize)
          workerCount should equal(fileToTest.workingActorsNeeded)
        }

        s"should be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on its BOM" in {
          val detection:Charset = Operations.detect(file.getAbsolutePath)
          detection should equal(fileToTest.encoding.charsetUsed)
        }

        s"should be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on its content" in {
          val detection = Converter.detectEncoding(file.getAbsolutePath)
          fileToTest.encoding.charsetUsed match {
            case charset if !charset.equals(BOMEncoding.ASCII.charsetUsed) => detection should equal(charset)
            case _ =>
          }
        }
      }
  }

  Seq(UTF8_with_BOM_error, UTF8_without_BOM_error, UTF16_BE_error, UTF16_LE_error, ASCII_error, Windows_1252_error)
    .foreach {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)
      var fileSize = 0l
      var workerCount = 0

      s"${fileToTest.fileName} file wrongly parameterized" must {
        s"Workers quantity should not be evaluated equals to ${fileToTest.workingActorsNeeded}" in {
          fileSize = file.length()
          workerCount = ParamAkka.numberOfWorkerRequired(fileSize)
          workerCount should not equal fileToTest.workingActorsNeeded
        }

        s"should not be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on its BOM" in {
          val detection = Operations.detect(file.getAbsolutePath)
          detection should not equal fileToTest.encoding
        }

        s"should not be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on its content" in {
          val detection = Converter.detectEncoding(file.getAbsolutePath)
          fileToTest.encoding.charsetUsed match {
            case charset if !charset.equals(BOMEncoding.ASCII.charsetUsed) => detection should not equal charset
            case _ =>
          }
        }
      }
  }

  Seq((UTF8_with_BOM, UTF8_with_BOM_bis), (UTF8_without_BOM, UTF8_without_BOM_bis), (UTF16_BE, UTF16_BE_bis), (UTF16_LE, UTF16_LE_bis)).foreach {
    case (first, second) =>
      val firstPath = encodedFileFolder + first.fileName
      val firstFile = new File(firstPath)
      val secondPath = encodedFileFolder + second.fileName
      val secondFile = new File(secondPath)
      val tempPath = s"${tempFilesFolder}merged_${first.fileName}_${second.fileName}"
      val tempFile = new File(tempPath)

      s"${first.fileName} and ${second.fileName}" must {
        "have the same detected BOM" in {
          val same = Operations.isSameEncoding(true, firstPath, secondPath)
          same should be(true)
        }

        s"Merge files ${first.fileName} and ${second.fileName} together" in {
          Operations.mergeFilesWithoutBom(tempPath, firstPath, secondPath)
          tempFile should be('exists)
          tempFile.length() should equal(firstFile.length() + secondFile.length() - first.encoding.BOM.size)
        }
      }
  }

  Seq((Windows_1252, ASCII), (UTF16_BE, UTF8_with_BOM), (UTF8_with_BOM, UTF16_LE), (UTF16_BE, UTF16_LE)).foreach {
    case (file1, file2) =>
      s"${file1.fileName} and ${file2.fileName}" must {
        "have different detected BOM" in {
          val same = Operations.isSameEncoding(false, encodedFileFolder + file1.fileName, encodedFileFolder + file2.fileName)
          same should be(false)
        }
      }
  }

  Seq((UTF8_with_BOM, UTF8_with_BOM_manually_cleaned), (UTF16_BE, UTF16_BE_manually_cleaned), (UTF16_LE, UTF16_LE_manually_cleaned), (ASCII, ASCII)).foreach {
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
          Operations.copyWithoutBom(sourcePath, tempFilePath, verbose = true)
          tempFile should be('exists)
          Operations.detect(tempFile.getAbsolutePath) should be(BOMEncoding.ASCII.charsetUsed)
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
          val file = new RandomAccessFile(tempFile, "rw")
          val channel = file.getChannel
          val lock = channel.tryLock()
          lock should not be null
          lock.release()
          channel.close()
          file.close()
          tempFile should (be('delete) and not be 'exists)
        }
      }
  }

  Seq(ASCII, UTF8_with_BOM, UTF16_BE, UTF16_LE, UTF8_with_BOM_bis, UTF8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis).foreach {
    file =>
      val fileToConvert = encodedFileFolder + file.fileName
      val convertedFile = tempFilesFolder + "converted_to_ASCII_" + file.fileName
      s"convert the file ${file.fileName} to ASCII" in {
        Converter.convert2ASCII(fileToConvert, convertedFile)
        new File(convertedFile) should be('exists)
        convertedFile.length should be > 0
        val encoding = Operations.detect(convertedFile)
        encoding should equal(BOMEncoding.ASCII.charsetUsed)
      }
  }

  Seq(UTF16_BE_bis, UTF16_LE_bis)
    .foreach {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)

      s"convert the file ${file.getName} to UTF-8" in {
        val fileConverted = new File(tempFilesFolder, "converted_to_UTF-8_" + fileToTest.fileName)
        Converter.convert2UTF_8(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding = Operations.detect(fileConverted.getAbsolutePath)
        encoding should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
        val encoding_bis = Converter.detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
      }

      s"convert the file ${file.getName} to ISO 8859-15" in {
        val fileConverted = new File(tempFilesFolder, "converted_to_ISO_8859-15_" + fileToTest.fileName)
        Converter.convert2ISO_8859_15(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding_bis = Converter.detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(Charset.forName("ISO-8859-1"))
      }
  }
}