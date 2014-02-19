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
import scala.io.Source
import java.io.{InputStream, FileOutputStream, OutputStreamWriter, BufferedWriter}


object Converter {
  private val convertAnyStringToASCII: String => String =
    text => Normalizer
      .normalize(text, Normalizer.Form.NFD)
      .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")

  def convert2ASCII(sourcePath: String, destinationPath: String, encodingSource: BOMFileEncoding, verbose: Boolean) = convert(BOM.removeBOM(verbose, encodingSource, sourcePath), destinationPath, encodingSource.charsetUsed, encodingDestination = StandardCharsets.US_ASCII, convertAnyStringToASCII)

  def convert(sourceIS: InputStream, destinationPath: String, encodingSource: Charset, encodingDestination: Charset, transformation: String => String) {
    val content = Source.fromInputStream(sourceIS, encodingSource.name())
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(destinationPath), encodingDestination.name()))
    try {
      val buffer = content.getLines()
      buffer
        .map(lineOfText => transformation(lineOfText) + (if (buffer.hasNext) "\n" else ""))
        .foreach(output.write)
    } finally {
      content.close()
      output.close()
    }
  }
}