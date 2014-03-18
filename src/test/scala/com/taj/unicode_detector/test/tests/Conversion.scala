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
import com.taj.unicode_detector.{BOMEncoding, Operations, Converter}
import java.io.File
import com.taj.unicode_detector.test.TestFile
import java.nio.charset.Charset
import com.taj.unicode_detector.HeuristicEncodingDetection._

object Conversion extends TestTrait {
  val test: TestFile => Unit = {
    file =>
      val fileToConvert = encodedFileFolder + file.fileName
      val convertedFile = s"${tempFilesFolder}converted_to_ASCII_from_${file.fileName}"
      s"convert the file ${file.fileName} to ASCII" in {
        Converter.convert2ASCII(fileToConvert, convertedFile)
        new File(convertedFile) should be('exists)
        convertedFile.length should be > 0
        val encoding = Operations.fullDetect(convertedFile)
        encoding should equal(BOMEncoding.ASCII.charsetUsed)
      }
  }

  val test2: TestFile => Unit = {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)

      s"convert the file ${file.getName} to UTF-8" in {
        val fileConverted = new File(tempFilesFolder, s"converted_to_UTF-8_from${fileToTest.fileName}")
        Converter.convert2UTF_8(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding = Operations.fullDetect(fileConverted.getAbsolutePath)
        encoding should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
        val encoding_bis = detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
      }

      s"convert the file ${file.getName} to ISO 8859-15" in {
        val fileConverted = new File(tempFilesFolder, s"converted_to_ISO_8859-15_from${fileToTest.fileName}")
        Converter.convert2ISO_8859_15(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding_bis = detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(Charset.forName("ISO-8859-1"))
      }
  }
}
