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

import java.text.Normalizer
import java.nio.charset.{StandardCharsets, Charset}
import scala.io.{BufferedSource, Source}
import java.io._
import com.ibm.icu.text.CharsetDetector
import scala.Some

/**
 * Convert text files to a specific format.
 */
object Converter {
  private val convertAnyStringToASCII: String => String =
    text => Normalizer
      .normalize(text, Normalizer.Form.NFD)
      .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

  private val noTransformation: String => String = line => line

  /**
   * Convert a text file to ASCII.
   * @param sourcePath path to the source file.
   * @param destinationPath path to the final file.
   * @param verbose print more information.
   */
  def convert2ASCII(sourcePath: String, destinationPath: String, verbose: Boolean) = convertWithTransformation(sourcePath, destinationPath, verbose, StandardCharsets.US_ASCII, convertAnyStringToASCII)

  /**
   * Convert from a format to UTF-8.
   * @param sourcePath path to the source file.
   * @param destinationPath path to the final file.
   * @param verbose print more information.
   */
  def convert2UTF_8(sourcePath: String, destinationPath: String, verbose: Boolean) = convertWithTransformation(sourcePath, destinationPath, verbose, StandardCharsets.UTF_8, noTransformation)

  /**
   * Convert from a format to ISO-8859-15.
   * @param sourcePath path to the source file.
   * @param destinationPath path to the final file.
   * @param verbose print more information.
   */
  def convert2ISO_8859_15(sourcePath: String, destinationPath: String, verbose: Boolean) = convertWithTransformation(sourcePath, destinationPath, verbose, Charset.forName("ISO-8859-15"), noTransformation)

  private def convertWithTransformation(sourcePath: String, destinationPath: String, verbose: Boolean, destinationEncoding: Charset, transformation: String => String) {
    val sourceEncoding: Charset = detectEncoding(sourcePath)
    val sourceIS: FileInputStream = BOMEncoding.getBOMfromCharset(sourceEncoding) match {
      case Some(bom) => Operations.removeBOM(verbose, bom, sourcePath)
      case None => new FileInputStream(sourcePath)
    }
    val content: BufferedSource = Source.fromInputStream(sourceIS, sourceEncoding.name())
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(destinationPath), destinationEncoding.name()))
    try {
      val buffer = content.getLines()
      buffer
        .map(lineOfText => transformation(lineOfText) + (if (buffer.hasNext) sys.props("line.separator") else ""))
        .foreach(output.write)
    } finally {
      content.close()
      output.close()
    }
  }

  /**
   * Detects the encoding of a text file based on Heuristic analyze.
   * @param path path to the file to analyze.
   * @return the name of the encoding as a String.
   */
  def detectEncoding(path: String): Charset = {
    val detector = new CharsetDetector()
    val byteData = new Array[Byte](1024 * 30)
    val is = new FileInputStream(path)
    try is.read(byteData)
    finally is.close()
    detector.setText(byteData)
    val matcher = detector.detect()
    Charset.forName(matcher.getName)
  }
}