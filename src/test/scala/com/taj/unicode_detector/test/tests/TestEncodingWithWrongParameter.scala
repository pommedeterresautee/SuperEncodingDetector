package com.taj.unicode_detector.test.tests

import java.io.File
import com.taj.unicode_detector.test.FirstListFilesToTest._
import com.taj.unicode_detector.{BOMEncoding, Converter, Operations, ParamAkka}
import com.taj.unicode_detector.test.TestFile
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import com.typesafe.scalalogging.slf4j.Logging


object TestEncodingWithWrongParameter extends WordSpecLike with Matchers with BeforeAndAfterAll with Logging {
  val test: TestFile => Unit = {
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
}
