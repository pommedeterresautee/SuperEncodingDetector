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

package com.taj.unicode_detector.test.tests

import com.taj.unicode_detector.test.FirstListFilesToTest._
import java.io.{ RandomAccessFile, FileInputStream, File }
import com.taj.unicode_detector.test.TestFile
import org.apache.commons.codec.digest.DigestUtils
import com.taj.unicode_detector.Encoding.BOM.BOMEncoding
import com.taj.unicode_detector.Encoding.Operations

object BOMTests extends TestTrait {
  val test: ((TestFile, TestFile)) ⇒ Unit = {
    case (first, second) ⇒
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

  val test2: ((TestFile, TestFile)) ⇒ Unit = {
    case (source, manuallyCleaned) ⇒
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
          Operations.fullDetect(tempFile.getAbsolutePath) should be(BOMEncoding.ASCII.charsetUsed)
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
