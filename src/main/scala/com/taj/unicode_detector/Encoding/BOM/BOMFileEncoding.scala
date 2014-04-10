package com.taj.unicode_detector.Encoding.BOM

import java.nio.charset.Charset

object BOMFileEncoding {
  def apply(charsetUsed: Charset) = new BOMFileEncoding(charsetUsed, List(), List())
}

/**
 * Contain properties of each BOM.
 * @param charsetUsed Encoding type.
 * @param BOM List of values of the first bytes when file is not an XML.
 * @param BOM_XML List of values of the first bytes when file is an XML.
 */
case class BOMFileEncoding(charsetUsed: Charset, BOM: List[Int], BOM_XML: List[Int])
