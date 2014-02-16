package com.taj.unicode_detector

case class EncodingType(name: String, hex:Seq[Int])

object Encoding {
  val bigUCS4           = EncodingType("UCS-4", Seq(0x00, 0x00, 0xFE, 0xFF))
  val littleUCS4        = EncodingType("UCS-4", Seq(0xFF, 0xFE, 0x00, 0x00))
  val unusualUCS4       = EncodingType("UCS-4", Seq(0x00, 0x00, 0xFF, 0xFE))
  val unusualUCS4_bis   = EncodingType("UCS-4", Seq(0xFE, 0xFF, 0x00, 0x00))
  val bigUTF16          = EncodingType("UTF-16BE", Seq(0xFE, 0xFF))
  val littleUTF16       = EncodingType("UTF-16LE", Seq(0xFF, 0xFE))
  val utf8              = EncodingType("UTF-8", Seq(0xEF, 0xBB, 0xBF))
}
