package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.BOM.BOMFileEncoding

/**
 * Defines the message to send an encoding detection result.
 */
object MessageResult {

  case class StartFileAnalyze()

  case class ResultOfTestBOM(result: Option[BOMFileEncoding])

}
