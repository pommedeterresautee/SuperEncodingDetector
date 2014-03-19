package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.BOM.BOMFileEncoding

/**
 * Created by geantvert on 19/03/14.
 */
object MessageResult {

  case class StartFileAnalyze()

  case class ResultOfTestBOM(result: Option[BOMFileEncoding])

}
