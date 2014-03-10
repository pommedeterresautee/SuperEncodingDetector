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

import java.io.File
import org.rogach.scallop._

object main extends App {

  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"

  val BIG_FILE = encodedFileFolder + "UTF8_without_BOM_big_file.txt"
  val arg = Array("--encoding", BIG_FILE, "--debug")
  val help = Array("--help")

  val opts = new ScallopConf(arg) {
    banner("""
                | ____                          _____                     _                 ____       _            _
                |/ ___| _   _ _ __   ___ _ __  | ____|_ __   ___ ___   __| (_)_ __   __ _  |  _ \  ___| |_ ___  ___| |_ ___  _ __
                |\___ \| | | |  _ \ / _ \ '__| |  _| | '_ \ / __/ _ \ / _` | | '_ \ / _` | | | | |/ _ \ __/ _ \/ __| __/ _ \| '__|
                | ___) | |_| | |_) |  __/ |    | |___| | | | (_| (_) | (_| | | | | | (_| | | |_| |  __/ ||  __/ (__| || (_) | |
                ||____/ \____| .__/ \___|_|    |_____|_| |_|\___\___/ \____|_|_| |_|\__, | |____/ \___|\__\___|\___|\__\___/|_|
                |            |_|                                                    |___/
                |
              		""".stripMargin + s"""
SuperEncodingDetector will help you to manage text files in different encoding format.
This application is good for working with the different Unicode version and ASCII character set but not to manage national specific code pages.

* Encoding detection is based on the Byte Order Mark (BOM) of the file if it is available (UTF-8, UTF-16 BE/LE and the two versions of  UTF-32 BE/LE).
* Encoding detection is based on a full scan of the text file if no BOM is available (UTF-8 and ASCII).
* Conversion from Unicode to ASCII is done by replacing special characters by their ASCII equivalents if possible.
* Merge different files encoded in a format including a BOM. The final file will include only one BOM.

Example: java -jar SuperEncodingDetector.jar --input .${File.separator}path1${File.separator}file1.txt .${File.separator}path2${File.separator}file2.txt .${File.separator}path3${File.separator}*.txt --detection

For usage see below:
           """)
    val filesExist: List[String] => Boolean = _.forall(new File(_).exists())

    val encoding = opt[List[String]]("encoding", descr = "Print the detected encoding of each file provided.", validate = filesExist)
    val removeBOM = opt[String]("removeBOM", descr = "Remove the Byte Order Mark from a file. Use output option to provide the destination folder.", validate = new File(_).exists())
    val convertUTF8 = opt[String]("convertUTF8", descr = "Convert a file from any format to UTF-8", validate = new File(_).exists())
    val convertASCII = opt[String]("convertASCII", descr = "Convert a file from Unicode encoding to ASCII", validate = new File(_).exists())
    val output = opt[String]("output", descr = "Path to the file where to save the result.", validate = !new File(_).exists())
    val merge = opt[List[String]]("merge", descr = "Merge the files provided. Use output option to provide the destination folder.", validate = filesExist)
    val debug = toggle("debug", descrYes = "Display lots of debug information during the process.", descrNo = "Display minimum during the process (same as not using this argument).", default = Some(false), prefix = "no-")
    val help = opt[Boolean]("help", descr = "Show this message.")
    // val version = opt[Boolean]("version", noshort = true, descr = "Print program version.")
    codependent(merge, output)
    codependent(convertASCII, output)
    conflicts(merge, List(encoding, help /*, version*/))
    conflicts(encoding, List(merge, help /*, version*/))
  }

  val debugOption = opts.debug.get
  val debug = debugOption match {
    case Some(true) =>
      println("Debug mode activated")
      true
    case _ => false
  }

  val optionDetection = opts.encoding.get
  optionDetection match {
    case Some(list) =>
      list.map(path => (path, Operations.detect(path, debug))).foreach {
        case (file, encoding) =>
          println(file + " ; " + encoding.charsetName)
      }
    case _ =>
  }

  val optionMerge = opts.merge.get
  optionMerge match {
    case Some(list) =>
      if (!Operations.isSameBOM(true, list: _*)) System.exit(1)
      Operations.mergeFilesWithoutBom(debug, opts.output.get.get, list: _*)
    case _ =>
  }
}