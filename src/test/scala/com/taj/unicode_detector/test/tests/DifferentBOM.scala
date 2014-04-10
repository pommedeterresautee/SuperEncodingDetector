package com.taj.unicode_detector.test.tests

import com.taj.unicode_detector.test.FirstListFilesToTest._
import com.taj.unicode_detector.test.TestFile
import com.taj.unicode_detector.Encoding.Operations

object DifferentBOM extends TestTrait {
  val test: ((TestFile, TestFile)) ⇒ Unit = {
    case (file1, file2) ⇒
      s"${file1.fileName} and ${file2.fileName}" must {
        "have different detected BOM" in {
          val same = Operations.isSameEncoding(false, encodedFileFolder + file1.fileName, encodedFileFolder + file2.fileName)
          same should be(false)
        }
      }
  }
}
