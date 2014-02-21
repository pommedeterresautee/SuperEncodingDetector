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

import java.io.{File, RandomAccessFile}

import akka.actor._
import akka.routing.RoundRobinRouter


sealed trait MessageAKKA

case class AnalyzeFile(path: String, verbose: Boolean) extends MessageAKKA

case class AnalyzeBlock(filePath: String, startRead: Long, length: Long, bufferSize: Int, testToOperate: Array[Byte] => Boolean, verbose: Boolean) extends MessageAKKA

case class Result(value: Boolean, verbose: Boolean) extends MessageAKKA

case class FinalFullCheckResult(isASCII: Boolean, timeElapsed: Long) extends MessageAKKA

object ParamAKKA {
  val bufferSize: Int = 1024 * 1024 * 10

  /**
   * Compute the number of AKKA workers needed to process the file ideally.
   * @param fileSize size of the file to process
   * @return the number of workers.
   */
  def numberOfWorkerRequired(fileSize: Long) = (1 to Runtime.getRuntime.availableProcessors)
    .find(_ * bufferSize >= fileSize)
    .getOrElse(Runtime.getRuntime.availableProcessors)

  val checkASCII: Array[Byte] => Boolean = _.map(_.toInt).forall(_ >= 0)

  val checkUTF8: Array[Byte] => Boolean = {
    byteArray =>
      val size = byteArray.takeWhile(_ != 0).size
      println(size)
      (0 to (size - 4))
        .map(i => (byteArray(i), byteArray(i + 1), byteArray(i + 2), byteArray(i + 3)))
        .exists {
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 192 & (b2 & 0xFF) >= 128 => true
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 224 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 => true
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 240 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 & (b4 & 0xFF) >= 128 => true
        case _ => false
      }
  }
}

class UTF8FileAnalyzer(totalLengthToAnalyze: Long) extends FileAnalyzer(totalLengthToAnalyze: Long, testToOperate = ParamAKKA.checkUTF8)

class ASCIIFileAnalyzer(totalLengthToAnalyze: Long) extends FileAnalyzer(totalLengthToAnalyze: Long, testToOperate = ParamAKKA.checkASCII)

class FileAnalyzer(totalLengthToAnalyze: Long, testToOperate: Array[Byte] => Boolean) extends Actor {

  // Determine the minimum of Workers depending of the size of the file and the size of the buffer.
  // If we are working on a small file, start less workers, if it s a big file, use the number of cores.
  val nbrOfWorkers = ParamAKKA.numberOfWorkerRequired(totalLengthToAnalyze)
  var masterSender: ActorRef = _
  val startTime = System.currentTimeMillis
  // to compute time elapsed to give a result
  val router = context.actorOf(Props[BlockAnalyzer].withRouter(RoundRobinRouter(nbrOfWorkers)), name = "workerRouter")
  val lengthPerWorkerToAnalyze = totalLengthToAnalyze / nbrOfWorkers
  var resultOfAnalyze = true
  // init
  var resultReceived = 0 // init

  def receive = {
    case AnalyzeFile(path, verbose) =>
      if (verbose) println("Start the processing...")
      if (verbose) println(s"received a message from $sender")
      masterSender = sender
      if (!new File(path).exists()) throw new IllegalArgumentException(s"Provided file doesn't exist: $path")
      (0 to nbrOfWorkers - 1)
        .foreach(workerNbr =>
        router ! AnalyzeBlock(path, workerNbr * lengthPerWorkerToAnalyze, lengthPerWorkerToAnalyze, ParamAKKA.bufferSize, testToOperate, verbose))
    case Result(isBlockASCII, verbose) =>
      resultReceived += 1
      resultOfAnalyze &= isBlockASCII
      if (resultReceived == nbrOfWorkers || !resultOfAnalyze) {
        if (verbose) println(s"send back the final result to $sender")
        masterSender ! FinalFullCheckResult(isBlockASCII, System.currentTimeMillis() - startTime)
        context.stop(self) // stop this actor and its children
        if (verbose) println("Finished the Akka process")
      }
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }
}

private class BlockAnalyzer extends Actor {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate, verbose) =>
      val ID = startRead / length
      if (verbose) println(s"Start analyze of block $ID [$startRead - ${startRead + length}[")
      sender ! analyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate, verbose)
      if (verbose) println(s"Stop analyze of block $ID")
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }

  private def analyzeBlock(path: String, startRead: Long, lengthOfBlockToAnalyze: Long, bufferSize: Integer, testToOperate: Array[Byte] => Boolean, verbose: Boolean): Result = {
    val limitToAnalyze = startRead + lengthOfBlockToAnalyze
    val randomAccessFile = new RandomAccessFile(path, "r")
    val buffer = new Array[Byte](bufferSize)

    var isASCII = false
    try {
      randomAccessFile.seek(startRead)
      isASCII = Iterator
        .continually(randomAccessFile.read(buffer))
        .takeWhile(c => c != -1
        && randomAccessFile.getFilePointer <= limitToAnalyze + bufferSize) // stop when the end of file || block is reached
        .map(_ => buffer) // buffer
        .forall(testToOperate)
    } finally {
      randomAccessFile.close()
    }
    Result(isASCII, verbose)
  }
}

//TODO find the location of the error if one is found
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

