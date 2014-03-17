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

package com.taj.unicode_detector

import java.nio.charset.{StandardCharsets, Charset}
import akka.actor.{Props, ActorRef, ActorSystem, Actor}
import java.io.FileInputStream
import com.taj.unicode_detector.ActorLife.{RegisterRootee, StartRegistration}

/**
 * Contain properties of each BOM.
 * @param charsetUsed Encoding type.
 * @param BOM List of values of the first bytes when file is not an XML.
 * @param BOM_XML List of values of the first bytes when file is an XML.
 */
case class BOMFileEncoding(charsetUsed: Charset, BOM: List[Int], BOM_XML: List[Int])

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
  //val UnknownEncoding = BOMFileEncoding(Charset.forName("") , List(), List()) //TODO replace by a setable charset at the construction of the instance

  /**
   * Detects the encoding of a file based on its BOM.
   * @param file path to the file.
   * @return the encoding. If no BOM detected, send back ASCII encoding.
   */
  def detect(file: String): Option[BOMFileEncoding] = {
    val in = new FileInputStream(file)
    val bytesToRead = 1024 // enough to read most XML encoding declarations

    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in mark bytesToRead
    val bytes = List(in.read, in.read, in.read, in.read)
    in.close() // To make the file deletable after processing!

    bytes match {
      case UTF32BE.BOM | UTF32BE.BOM_XML => Some(UTF32BE)
      case UTF32LE.BOM | UTF32LE.BOM_XML => Some(UTF32LE)
      case UTF32LEUnusual.BOM | UTF32LEUnusual.BOM_XML => Some(UTF32LEUnusual)
      case UTF32BEUnusual.BOM | UTF32BEUnusual.BOM_XML => Some(UTF32BEUnusual)
      case UTF_16_BE.BOM :+ _ :+ _ | UTF_16_BE.BOM_XML => Some(UTF_16_BE)
      case UTF_16_LE.BOM :+ _ :+ _ | UTF_16_LE.BOM_XML => Some(UTF_16_LE)
      case UTF8.BOM :+ _ | UTF8.BOM_XML => Some(UTF8)
      case _ => None
    }
  }
}

object BOMBasedDetectionActor {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new BOMBasedDetectionActor(path)), "BOMActor")
  }
}

/**
 * This class detects the encoding of a file based on its BOM.
 */
class BOMBasedDetectionActor(file: String) extends Actor {

  import BOMEncoding._
  import TestResult._

  def receive = {
    case StartRegistration(register) =>
      register ! RegisterRootee(self)
    case InitAnalyzeFile() =>
      sender ! ResultOfTestBOM(detect(file))
    case _ => throw new IllegalArgumentException(s"Failed to retrieve result from ${self.path} during BOM detection")
  }
}