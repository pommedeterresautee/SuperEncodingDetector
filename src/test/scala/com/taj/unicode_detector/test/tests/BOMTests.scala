package com.taj.unicode_detector.test.tests

import com.taj.unicode_detector.test.FirstListFilesToTest._
import java.io.{RandomAccessFile, FileInputStream, File}
import com.taj.unicode_detector.{BOMEncoding, Operations}
import com.taj.unicode_detector.test.TestFile
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import com.typesafe.scalalogging.slf4j.Logging
import org.apache.commons.codec.digest.DigestUtils


object BOMTests extends WordSpecLike with Matchers with BeforeAndAfterAll with Logging {
  val test: ((TestFile, TestFile)) => Unit = {
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

  val test2: ((TestFile, TestFile)) => Unit = {
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
}
