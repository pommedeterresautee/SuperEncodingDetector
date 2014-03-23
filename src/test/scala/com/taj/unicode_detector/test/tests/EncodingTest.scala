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

import java.io.File
import com.taj.unicode_detector.test.FirstListFilesToTest._
import com.taj.unicode_detector.ParamAkka
import java.nio.charset.Charset
import com.taj.unicode_detector.test.TestFile
import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import HeuristicEncodingDetection._
import com.taj.unicode_detector.Encoding.BOM.BOMEncoding
import com.taj.unicode_detector.Encoding.Operations


object EncodingTest extends TestTrait {
  val test: TestFile => Unit = {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)
      var fileSize = 0l
      var workerCount = 0

      s"${fileToTest.fileName} file" must {
        s"Workers quantity should be evaluated equals to ${fileToTest.workingActorsNeeded}." in {
          fileSize = file.length()
          workerCount = ParamAkka.numberOfWorkerRequired(fileSize)
          workerCount should equal(fileToTest.workingActorsNeeded)
        }

        s"should be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on its BOM." in {
          val detection: Charset = Operations.fullDetect(file.getAbsolutePath)
          detection should equal(fileToTest.encoding.charsetUsed)
        }

        s"should be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on an Heuristic analyze only." in {
          val detection = detectEncoding(file.getAbsolutePath)
          fileToTest.encoding.charsetUsed match {
            case charset if !charset.equals(BOMEncoding.ASCII.charsetUsed) => detection should equal(charset)
            case _ =>
          }
        }

        s"should be detected as encoded with charset ${fileToTest.encoding.charsetUsed} based on BOM analyze then heuristic analyze." in {

          //          val probe = TestProbe()
          //          val actor = MiniDetectionTest(file.getAbsolutePath, probe.ref)


          val detection = Operations.miniDetect(file.getAbsolutePath, None)
          fileToTest.encoding.charsetUsed match {
            case charset if !charset.equals(BOMEncoding.ASCII.charsetUsed) => detection should equal(charset)
            case _ =>
          }
        }
      }
  }
}
