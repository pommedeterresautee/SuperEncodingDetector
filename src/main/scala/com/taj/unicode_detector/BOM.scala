package com.taj.unicode_detector

import java.io.FileInputStream

/**
 * Contain property of each BOM.
 * @param name Name of the encoding type.
 * @param BOM List of values of the first bytes when file is not an XML.
 * @param BOM_XML List of values of the first bytes when file is an XML.
 */
case class FileEncoding(name: String, BOM: List[Int], BOM_XML: List[Int])

/**
 * Main class to detect a file encoding based on its BOM.
 */
object BOM {
  val bigUCS4 = FileEncoding("UCS-4", List(0x00, 0x00, 0xFE, 0xFF), List(0x00, 0x00, 0x00, '<'))
  val littleUCS4 = FileEncoding("UCS-4", List(0xFF, 0xFE, 0x00, 0x00), List('<', 0x00, 0x00, 0x00))
  val unusualUCS4 = FileEncoding("UCS-4", List(0x00, 0x00, 0xFF, 0xFE), List(0x00, 0x00, '<', 0x00))
  val unusualUCS4_bis = FileEncoding("UCS-4", List(0xFE, 0xFF, 0x00, 0x00), List(0x00, '<', 0x00, 0x00))
  val UTF16BE = FileEncoding("UTF-16BE", List(0xFE, 0xFF), List(0x00, '<', 0x00, '?'))
  val UTF16LE = FileEncoding("UTF-16LE", List(0xFF, 0xFE), List('<', 0x00, '?', 0x00))
  val UTF8 = FileEncoding("UTF-8", List(0xEF, 0xBB, 0xBF), List(0x4C, 0x6F, 0xA7, 0x94))
  //val UTF8_WITHOUT_BOM  = BOM("UTF-8", List(), List('<' , '?' , 'x' , 'm' ))
  val ASCII = FileEncoding("ASCII", List(), List('<', '?', 'x', 'm'))

  /**
   * Detects the encoding of a file based on its BOM.
   * @param file path to the file.
   * @return the encoding. If no BOM detected, send back ASCII encoding.
   */
  def detect(file: String): FileEncoding = {
    val in = new FileInputStream(file)
    var ret: FileEncoding = null
    val bytesToRead = 1024 // enough to read most XML encoding declarations

    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in mark bytesToRead
    val bytes = List(in.read, in.read, in.read, in.read)

    ret = bytes match {
      case bigUCS4.BOM            | bigUCS4.BOM_XML           => bigUCS4
      case littleUCS4.BOM         | littleUCS4.BOM_XML        => littleUCS4
      case unusualUCS4.BOM        | unusualUCS4.BOM_XML       => unusualUCS4
      case unusualUCS4_bis.BOM    | unusualUCS4_bis.BOM_XML   => unusualUCS4_bis
      case UTF16BE.BOM :+ _ :+ _  | UTF16BE.BOM_XML           => UTF16BE
      case UTF16LE.BOM :+ _ :+ _  | UTF16LE.BOM_XML           => UTF16LE
      case UTF8.BOM :+ _          | UTF8.BOM_XML              => UTF8
      case _                                                  => ASCII
    }
    ret
  }
}