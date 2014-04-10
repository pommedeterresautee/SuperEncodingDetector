package com.taj.unicode_detector.Encoding.BOM

import java.nio.charset.{ StandardCharsets, Charset }
import java.io.FileInputStream

/**
 * List all the encoding types detected.
 */
object BOMEncoding {

  /**
   * Get a list of existing BOMs.
   * @return the different sorts of BOMs
   */
  def values = Seq(UTF32BE, UTF32LE, UTF32BEUnusual, UTF32LEUnusual, UTF_16_BE, UTF_16_LE, UTF8, UTF8NoBOM, ASCII)

  def getBOMfromCharset(charset: Charset): Option[BOMFileEncoding] = values.find(_.charsetUsed.name().contentEquals(charset.name()))

  val UTF32BE = BOMFileEncoding(Charset.forName("UTF-32BE"), List(0x00, 0x00, 0xFE, 0xFF), List(0x00, 0x00, 0x00, '<'))
  val UTF32LE = BOMFileEncoding(Charset.forName("UTF-32LE"), List(0xFF, 0xFE, 0x00, 0x00), List('<', 0x00, 0x00, 0x00))
  val UTF32BEUnusual = BOMFileEncoding(Charset.forName("x-UTF-32BE-BOM"), List(0xFE, 0xFF, 0x00, 0x00), List(0x00, '<', 0x00, 0x00))
  val UTF32LEUnusual = BOMFileEncoding(Charset.forName("x-UTF-32LE-BOM"), List(0x00, 0x00, 0xFF, 0xFE), List(0x00, 0x00, '<', 0x00))
  val UTF_16_BE = BOMFileEncoding(StandardCharsets.UTF_16BE, List(0xFE, 0xFF), List(0x00, '<', 0x00, '?'))
  val UTF_16_LE = BOMFileEncoding(StandardCharsets.UTF_16LE, List(0xFF, 0xFE), List('<', 0x00, '?', 0x00))
  val UTF8 = BOMFileEncoding(StandardCharsets.UTF_8, List(0xEF, 0xBB, 0xBF), List(0x4C, 0x6F, 0xA7, 0x94))
  val UTF8NoBOM = BOMFileEncoding(StandardCharsets.UTF_8, List(), List('<', '?', 'x', 'm'))
  val ASCII = BOMFileEncoding(StandardCharsets.US_ASCII, List(), List('<', '?', 'x', 'm'))

  /**
   * Detects the encoding of a file based on its BOM.
   * @param file path to the file.
   * @return the encoding. If no BOM detected, send back ASCII encoding.
   */
  def detectBOM(file: String): Option[BOMFileEncoding] = {
    val in = new FileInputStream(file)
    val bytesToRead = 1024 // enough to read most XML encoding declarations

    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in mark bytesToRead
    val bytes = List(in.read, in.read, in.read, in.read)
    in.close() // To make the file deletable after processing!

    bytes match {
      case UTF32BE.BOM | UTF32BE.BOM_XML               ⇒ Some(UTF32BE)
      case UTF32LE.BOM | UTF32LE.BOM_XML               ⇒ Some(UTF32LE)
      case UTF32LEUnusual.BOM | UTF32LEUnusual.BOM_XML ⇒ Some(UTF32LEUnusual)
      case UTF32BEUnusual.BOM | UTF32BEUnusual.BOM_XML ⇒ Some(UTF32BEUnusual)
      case UTF_16_BE.BOM :+ _ :+ _ | UTF_16_BE.BOM_XML ⇒ Some(UTF_16_BE)
      case UTF_16_LE.BOM :+ _ :+ _ | UTF_16_LE.BOM_XML ⇒ Some(UTF_16_LE)
      case UTF8.BOM :+ _ | UTF8.BOM_XML                ⇒ Some(UTF8)
      case _                                           ⇒ None
    }
  }
}
