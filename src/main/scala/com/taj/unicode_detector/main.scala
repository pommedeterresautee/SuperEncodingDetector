/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 TAJ - Société d'avocats
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
 */

package com.taj.unicode_detector

import java.io.{File, FileInputStream}


object main extends App {

  val pathToFEC = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\FEC.TXT"
  val pathToTest = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\FEC_TEST.TXT"
  val percentage = 10

  var bytesToRead = new File(pathToFEC).length() * percentage / 100

  if(Int.MaxValue > bytesToRead) {
    println(s"According to the parameters you have requested to test ${bytesToRead / (1024 * 1024)}Mb.\n" +
      s"The analyze will be limited to ${Int.MaxValue / (1024 * 1024)}Mb")
    bytesToRead = Int.MaxValue
  }

  val is = new FileInputStream(pathToFEC)

  val isASCII = Iterator
    .continually(is.read())
    .take(bytesToRead.toInt)
    .forall(_ <= 127)
  //.forall(mChar => !(Character.UnicodeBlock.of(mChar)==Character.UnicodeBlock.BASIC_LATIN))

  println(if (isASCII) "ascii" else "non ascii")
}
