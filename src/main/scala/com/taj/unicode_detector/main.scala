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
import akka.actor.{Props, ActorSystem}

object main extends App {


  val pathToFile = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\test.txt"
  val percentageToAnalyze = 100

  val bytesToRead = new File(pathToFile).length() * percentageToAnalyze / 100

  // Determine the minimum of Workers depending of the size of the file and the size of the buffer.
  // If we are working on a small file, start less workers, if it s a big file, use the number of cores.
  val workerCount = (1 to Runtime.getRuntime.availableProcessors).find(_ * ParamAKKA.bufferSize >= bytesToRead).getOrElse(Runtime.getRuntime.availableProcessors)

  val system = ActorSystem("AsciiDetector")

  val logger = system.actorOf(Props[TheLogger], name = "LoggerWorker")
  val master = system.actorOf(Props(new FileAnalyzer(logger, workerCount, bytesToRead)), name = "FileAnalyzer")

  master ! AnalyzeFile(pathToFile)
}

/*
* isASCII = Iterator
        .continually(randomAccessFile.read(buffer))
        .takeWhile(c => c != -1
        && randomAccessFile.getFilePointer <= limitToAnalyze + main.bufferSize) // stop when the end of file || block is reached
        .zipWithIndex
        .flatMap{case(_, bufferCounter) => buffer.map((_, bufferCounter))}
        .zipWithIndex
        .map{case((byte, bufferCounter), arrayCounter) => (byte, bufferCounter, arrayCounter)}
        .find{case(testedByte, bufferCounter, arrayCounter) => testedByte < 0 && testedByte > 127}
* */