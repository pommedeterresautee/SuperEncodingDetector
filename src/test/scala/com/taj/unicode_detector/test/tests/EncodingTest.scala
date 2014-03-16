package com.taj.unicode_detector.test.tests

import java.io.File
import com.taj.unicode_detector.test.FirstListFilesToTest._
import com.taj.unicode_detector.{BOMEncoding, Converter, Operations, ParamAkka}
import java.nio.charset.Charset
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.test.TestFile


object EncodingTest extends WordSpecLike with Matchers with BeforeAndAfterAll with Logging {
  val test: TestFile => Unit = {
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
          val detection: Charset = Operations.detect(file.getAbsolutePath)
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
}
