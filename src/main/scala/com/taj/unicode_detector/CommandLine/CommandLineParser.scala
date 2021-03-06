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

package com.taj.unicode_detector.CommandLine

import java.io.File
import org.rogach.scallop.ScallopConf

object CommandLineParser {
  def apply(args: Array[String]) = new CommandLineParser(args)
}

/**
 * Parse the argument provided on command line.
 * @param args arguments provided by the command line.
 */
class CommandLineParser(args: Array[String]) extends ScallopConf(args: Array[String]) {
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

  footer("\nThis application has been brought to you by Taj - Société d'avocats.")

  version(s"Super Encoding Detector - version ${com.taj.unicode_detector.CommandLine.Main.getClass.getPackage.getImplementationVersion}")

  val filesExist: List[String] ⇒ Boolean = _.forall {
    new File(_).isFile
  }

  val encoding = opt[List[String]]("encoding", descr = "Print the detected encoding of each file provided.", validate = filesExist)
  //val removeBOM = opt[String]("removeBOM", descr = "Remove the Byte Order Mark from a file. Use output option to provide the destination folder.", validate = new File(_).exists())
  val convert8859_15 = opt[List[String]]("ISO8859convert", descr = "Convert a file from any format to ISO 8859-15. Use with outputFolder.", validate = filesExist)
  val convertUTF8 = opt[List[String]]("UTF8convert", descr = "Convert a file from any format to UTF-8. Use with outputFolder.", validate = filesExist)
  //val convertASCII = opt[List[String]]("ASCIIconvert", descr = "Convert a file from Unicode encoding to ASCII. Use with outputFolder.", validate = filesExist)
  val output = opt[String]("output", descr = "Path to the file where to save the result.")
  val outputFolder = opt[String]("outputFolder", descr = "Path to the folder where to save the conversion results.", validate = new File(_).isDirectory)
  val merge = opt[List[String]]("merge", descr = "Merge the files provided. Use output option to provide the destination folder.", validate = filesExist)
  val debug = toggle("debug", descrYes = "Display lots of debug information during the process.", descrNo = "Display minimum during the process (same as not using this argument).", default = Some(false), prefix = "no-")
  val help = opt[Boolean]("help", descr = "Show this message.")
  val version = opt[Boolean]("version", noshort = true, descr = "Print program version.")
  dependsOnAll(merge, List(outputFolder))
  dependsOnAll(convertUTF8, List(outputFolder))
  dependsOnAll(convert8859_15, List(outputFolder))
  //    dependsOnAll(convertASCII, List(outputFolder))

  conflicts(merge, List(encoding, help))
  conflicts(encoding, List(merge, help))
}
