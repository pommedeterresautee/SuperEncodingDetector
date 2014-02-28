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

import java.io.{File, FileOutputStream, FileInputStream}
import java.nio.file.{Paths, Files}
import java.nio.charset.{Charset, StandardCharsets}
import akka.actor.{Props, ActorSystem}
import akka.util.Timeout
import scala.concurrent.Await
import akka.pattern.ask
import java.util.concurrent.TimeUnit


/**
 * Contain properties of each BOM.
 * @param charsetUsed Encoding type.
 * @param BOM List of values of the first bytes when file is not an XML.
 * @param BOM_XML List of values of the first bytes when file is an XML.
 */
case class BOMFileEncoding(charsetUsed: Option[Charset], BOM: List[Int], BOM_XML: List[Int])

/**
 * Main class to detect a file encoding based on its BOM.
 */
object BOM {
  val UTF32BE = BOMFileEncoding(Some(Charset.forName("UTF-32BE")), List(0x00, 0x00, 0xFE, 0xFF), List(0x00, 0x00, 0x00, '<'))
  val UTF32LE = BOMFileEncoding(Some(Charset.forName("UTF-32LE")), List(0xFF, 0xFE, 0x00, 0x00), List('<', 0x00, 0x00, 0x00))
  val UTF32BEUnusual = BOMFileEncoding(Some(Charset.forName("x-UTF-32BE-BOM")), List(0xFE, 0xFF, 0x00, 0x00), List(0x00, '<', 0x00, 0x00))
  val UTF32LEUnusual = BOMFileEncoding(Some(Charset.forName("x-UTF-32LE-BOM")), List(0x00, 0x00, 0xFF, 0xFE), List(0x00, 0x00, '<', 0x00))
  val UTF_16_BE = BOMFileEncoding(Some(StandardCharsets.UTF_16BE), List(0xFE, 0xFF), List(0x00, '<', 0x00, '?'))
  val UTF_16_LE = BOMFileEncoding(Some(StandardCharsets.UTF_16LE), List(0xFF, 0xFE), List('<', 0x00, '?', 0x00))
  val UTF8 = BOMFileEncoding(Some(StandardCharsets.UTF_8), List(0xEF, 0xBB, 0xBF), List(0x4C, 0x6F, 0xA7, 0x94))
  val UTF8NoBOM = BOMFileEncoding(Some(StandardCharsets.UTF_8), List(), List('<', '?', 'x', 'm'))
  val ASCII = BOMFileEncoding(Some(StandardCharsets.US_ASCII), List(), List('<', '?', 'x', 'm'))
  val UnknownEncoding = BOMFileEncoding(None, List(), List())

  /**
   * Detects the encoding of a file based on its BOM.
   * @param file path to the file.
   * @return the encoding. If no BOM detected, send back ASCII encoding.
   */
  def detect(file: String, verbose: Boolean): BOMFileEncoding = {
    val in = new FileInputStream(file)
    var ret: BOMFileEncoding = null
    val bytesToRead = 1024 // enough to read most XML encoding declarations

    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in mark bytesToRead
    val bytes = List(in.read, in.read, in.read, in.read)
    in.close() // To make the file deletable after processing!

    ret = bytes match {
      case UTF32BE.BOM | UTF32BE.BOM_XML => UTF32BE
      case UTF32LE.BOM | UTF32LE.BOM_XML => UTF32LE
      case UTF32LEUnusual.BOM | UTF32LEUnusual.BOM_XML => UTF32LEUnusual
      case UTF32BEUnusual.BOM | UTF32BEUnusual.BOM_XML => UTF32BEUnusual
      case UTF_16_BE.BOM :+ _ :+ _ | UTF_16_BE.BOM_XML => UTF_16_BE
      case UTF_16_LE.BOM :+ _ :+ _ | UTF_16_LE.BOM_XML => UTF_16_LE
      case UTF8.BOM :+ _ | UTF8.BOM_XML => UTF8
      case _ => // No BOM detected

        val system: ActorSystem = ActorSystem("ActorSystemFileIdentification")

        val masterASCII = system.actorOf(Props(new ASCIIFileAnalyzer(verbose, file)), name = "ASCIIFileAnalyzer")
        implicit val timeout = Timeout(2, TimeUnit.MINUTES)
        val result: BOMFileEncoding = Await.result(masterASCII ? InitAnalyzeFile(), timeout.duration) match {
          case FullCheckResult(None, _) =>
            ASCII
          case FullCheckResult(Some(positionNonASCIIByte), _) =>
            val masterUTF8 = system.actorOf(Props(new UTF8FileAnalyzer(verbose, file)), name = "UTF8FileAnalyzer")
            Await.result(masterUTF8 ? InitAnalyzeFile(), timeout.duration) match {
              case FullCheckResult(None, _) => UTF8NoBOM
              case FullCheckResult(Some(positionNonUTF8Byte), _) =>
                if (verbose) println(s"The first non matching byte is located at position $positionNonUTF8Byte in the file $file")
                UnknownEncoding
              case _ => throw new IllegalArgumentException("Failed to retrieve result from Actor during ASCII check")
            }
          case _ => throw new IllegalArgumentException("Failed to retrieve result from Actor during UTF8 no BOM check")
        }
        println("Shutdown the Akka system")
        system.shutdown()
        result
    }
    ret
  }

  /**
   * Compare files given in parameter to the BOM in parameters to determine if they are all the same.
   * @param verbose print some helpful messages.
   * @param paths paths to the files to compare.
   * @return true if the encoding is the same everywhere.
   */
  def isSameBOM(verbose: Boolean, paths: String*): Boolean = {
    if (paths.size < 2) throw new IllegalArgumentException(s"Not enough files to compare (${paths.size})")
    val bom: BOMFileEncoding = detect(paths.head, verbose)
    paths.tail.forall {
      path: String =>
        val detectedBOM = detect(path, verbose)
        val same = bom.equals(detectedBOM)
        if (!same && verbose) println(s"The first file [${paths.head}] is encoded as ${bom.charsetUsed} but the file [$path] is encoded as ${detectedBOM.charsetUsed}.")
        same
    }
  }

  /**
   * Remove the bytes relative to the detected BOM of an existing text file.
   * @param verbose true if want to see the progress of the process.
   * @param bom the BOM to remove to the file.
   * @param path the path to the file to the file.
   * @return
   */
  def removeBOM(verbose: Boolean, bom: BOMFileEncoding, path: String): FileInputStream = {
    val toDrop = bom.BOM.size
    val f = new FileInputStream(path)
    val realSkipped = f.skip(toDrop)
    if (toDrop != realSkipped) throw new IllegalStateException(s"Failed to skip the correct number of bytes for the file $path ($realSkipped instead of $toDrop)")
    f
  }

  /**
   * Copy a file to another place without its BOM.
   * @param from path to the source.
   * @param to path to the destination.
   * @param verbose true to see more information about the process ongoing.
   */
  def copyWithoutBom(from: String, to: String, verbose: Boolean) {
    val bomFrom = detect(from, verbose)
    val fileTo = new File(to)
    if (fileTo.exists()) fileTo.delete()
    if (fileTo.exists()) throw new IllegalStateException(s"File $to can't be deleted.")
    val output = new FileOutputStream(fileTo)

    val input = removeBOM(verbose, bomFrom, from)
    val bytes = new Array[Byte](1024) //1024 bytes - Buffer size
    try {
      Iterator
        .continually(input.read(bytes))
        .takeWhile(-1 !=)
        .foreach(read => output.write(bytes, 0, read))
    } finally {
      input.close()
      output.close()
    }
  }

  /**
   * Merge several text files together even if they have a BOM.
   * @param verbose true to display the process ongoing.
   * @param destination path where to save the merged file.
   * @param paths paths to the files to merge together.
   */
  def mergeFilesWithoutBom(verbose: Boolean, destination: String, paths: String*) {
    Files.copy(Paths.get(paths(0)), Paths.get(destination))
    val bytes = new Array[Byte](1024)
    val output = new FileOutputStream(destination, true)
    try paths.drop(1).foreach {
      path =>
        val input = removeBOM(verbose, detect(path, verbose), path)
        try Iterator
          .continually(input.read(bytes))
          .takeWhile(-1 !=)
          .foreach(read => try output.write(bytes, 0, read))
        finally input.close()
    } finally output.close()
  }
}