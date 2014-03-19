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

import java.io.{BufferedWriter, FileWriter, File}
import org.slf4j.impl.SimpleLogger
import com.typesafe.scalalogging.slf4j.Logging

object main extends App with Logging {

  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"

  val BIG_FILE = encodedFileFolder + "UTF8_without_BOM_big_file.txt"
  val SECOND_FILE = encodedFileFolder + "UTF16_LE.txt"
  val arg = Array("--encoding", BIG_FILE, SECOND_FILE)
  val help = Array("--help")

  val opts = new CommandLineParser(args)

  System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, if (opts.debug.get.getOrElse(false)) "debug" else "info")

  // delete existing output file
  opts.output.get.map(new File(_)).filter(_.exists()).foreach(_.delete())

  opts.encoding.get
    .map(
      _.map(path => (new File(path).getName, Operations.miniDetect(path)))
        .map {
        case (fileName, encoding) => fileName + "|" + encoding.name()
      }
        .foreach {
        case result if opts.output.get.isEmpty => println(result)
        case result if opts.output.get.isDefined =>
          val w = new BufferedWriter(new FileWriter(opts.output.get.get, true))
          w.write(result)
          w.newLine()
          w.close()
        case _ => throw new IllegalArgumentException("Wrong argument provided")
      })

  val convert8859_15 = opts.convert8859_15.get
  convert8859_15 match {
    case Some(list: List[String]) =>
      list.foreach(file => Converter.convert2ISO_8859_15(file,
        new File(opts.output.get.get, new File(file).getName).getAbsolutePath))
    case None =>
  }

  val convertUTF8 = opts.convertUTF8.get
  convertUTF8 match {
    case Some(list: List[String]) =>
      list.foreach(file => Converter.convert2UTF_8(file,
        new File(opts.output.get.get, new File(file).getName).getAbsolutePath))
    case None =>
  }

  val optionMerge = opts.merge.get
  optionMerge match {
    case Some(list) =>
      if (!Operations.isSameEncoding(true, list: _*)) System.exit(1)
      Operations.mergeFilesWithoutBom(opts.output.get.get, list: _*)
    case None =>
  }
}