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
import akka.routing.{Broadcast, RoundRobinRouter}
import scala.Option
import akka.util.Timeout
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ArrayBuffer
import com.taj.unicode_detector.ActorLifeOverview._
import com.taj.unicode_detector.EncodingAnalyze._

object EncodingAnalyze{
  case class InitAnalyzeFile()
  case class AnalyzeBlock(filePath: String, startRead: Long, length: Long, bufferSize: Int, testToOperate: Array[Byte] => Int)
  case class Result(actor: ActorRef, pathOfTheFileAnalyzed: String, nonMatchingCharPositionInFile: Option[Long], verbose: Boolean)
  case class FullCheckResult(nonMatchingBytePositionInFile: Option[Long], timeElapsed: Long)
}


object ParamAKKA {
  val bufferSize: Int = 1024

  /**
   * Size of each part sent to each Actor.
   * Speed test for different parameters based on a 400 Mb file (time in ms).
   * Size   Time
   * 2   Kb 151 021
   * 1   Mb  25 205
   * 10  Mb  22 207
   * 20  Mb  21 384 <- Best
   * 70  Mb  22 691
   * 100 Mb  23 671
   */
  val sizeOfaPartToAnalyze = 1024 * 1024 * 20

  /**
   * Compute the number of AKKA workers needed to process the file.
   * Computation based on the size of the file and the size of the segments to analyze.
   * If we are working on a small file, start less workers, if it s a big file, use the number of processor cores.
   * @param fileSize size of the file to process
   * @return the number of workers.
   */
  def numberOfWorkerRequired(fileSize: Long) =
    (1 to Runtime.getRuntime.availableProcessors)
      .find(_ * sizeOfaPartToAnalyze >= fileSize)
      .getOrElse(Runtime.getRuntime.availableProcessors)

  val checkASCII: Array[Byte] => Int = _.indexWhere(_.toInt < 0)

  val checkUTF8: Array[Byte] => Int = {
    byteArray =>
      val realArraySize = byteArray.takeWhile(_ != 0).size
      var passBytesAlreadyMatched = 4 // the first 4 bytes of the block are passed in case they are related to a truncated unicode char from another block
    var passBytesMayMatch = -1 // if not yet the entire sequence, wait to advance 4 bytes to say if there is definitely no match
      (0 to (realArraySize - 4))
        .map(i => (byteArray(i), byteArray(i + 1), byteArray(i + 2), byteArray(i + 3)))
        .indexWhere {
        case (b1, b2, b3, b4) if b1.toInt >= 0 & b2.toInt >= 0 & b3.toInt >= 0 & b4.toInt >= 0 => // ASCII
          passBytesMayMatch = -1
          passBytesAlreadyMatched = 0
          false
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 192 & (b2 & 0xFF) >= 128 =>
          passBytesMayMatch = -1
          passBytesAlreadyMatched = 1
          false
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 224 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 =>
          passBytesMayMatch = -1
          passBytesAlreadyMatched = 2
          false
        case (b1, b2, b3, b4) if (b1 & 0xFF) >= 240 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 & (b4 & 0xFF) >= 128 =>
          passBytesMayMatch = -1
          passBytesAlreadyMatched = 3
          false
        case (b1, b2, b3, b4) =>
          if (passBytesAlreadyMatched > 0) {
            passBytesAlreadyMatched -= 1
            false
          } else if (passBytesMayMatch == -1) {
            passBytesMayMatch = 3 // pass the check for the next 3 bytes to get a sequence of 4 bytes.
            false
          } else if (passBytesMayMatch > 0) {
            passBytesMayMatch -= 1 // decrease the count waiting for the entire sequence
            false
          } else {
            true
          }
      }
  }
}

class UTF8FileAnalyzer(verbose: Boolean, path: String) extends FileAnalyzer(verbose: Boolean, path: String, testToOperate = ParamAKKA.checkUTF8)

class ASCIIFileAnalyzer(verbose: Boolean, path: String) extends FileAnalyzer(verbose: Boolean, path: String, testToOperate = ParamAKKA.checkASCII)

class FileAnalyzer(verbose: Boolean, path: String, testToOperate: Array[Byte] => Int) extends Actor {
  val totalLengthToAnalyze = new File(path).length()
  val numberOfPartToAnalyze = (totalLengthToAnalyze / ParamAKKA.sizeOfaPartToAnalyze).toInt match {
    case 0 => 1
    case count: Int => count
  }

  val nbrOfWorkers = ParamAKKA.numberOfWorkerRequired(totalLengthToAnalyze)
  val routerBlockAnalyzer: ActorRef = context.actorOf(Props(new BlockAnalyzer(verbose)).withRouter(RoundRobinRouter(nbrOfWorkers)), name = s"Router_${self.path.name.charAt(0)}")
  
  var masterSender: Option[ActorRef] = None
  var startAnalyzeTime = 0l
  var resultReceived = 0

  def receive = {
    case StartRegistration(registrer) =>
      registrer ! RegisterRooter(routerBlockAnalyzer)
      routerBlockAnalyzer ! Broadcast(RegisterMe(registrer))
    
    case InitAnalyzeFile() =>
      startAnalyzeTime = System.currentTimeMillis
      masterSender = Some(sender)

      if (verbose) println(
        s"""Start processing @$startAnalyzeTime
        Current actor [${self.path}]
        Received a message from ${sender.path}.
        Will use $nbrOfWorkers Workers."""
      )

      // Initialization of the workers
      (0 to nbrOfWorkers - 1)
        .foreach(partNumber =>
        routerBlockAnalyzer ! AnalyzeBlock(path, partNumber * ParamAKKA.sizeOfaPartToAnalyze, ParamAKKA.sizeOfaPartToAnalyze, ParamAKKA.bufferSize, testToOperate))

      if (verbose) println(s"The sender is ${sender.path} but the parent is ${context.parent.path}")

    case Result(actor, filePath, nonMatchingCharPositionInFile, verboseActivated) =>
      resultReceived += 1

      masterSender match {
        case None =>
        case Some(masterActor) if resultReceived == numberOfPartToAnalyze || !nonMatchingCharPositionInFile.isEmpty =>
          masterActor ! FullCheckResult(nonMatchingCharPositionInFile, System.currentTimeMillis() - startAnalyzeTime)
          implicit val timeout = Timeout(2, TimeUnit.MINUTES)
          masterSender = None

          routerBlockAnalyzer ! Broadcast(PoisonPill)

          if (verboseActivated) println(s"*** Finished Actor ${self.path} process in ${System.currentTimeMillis() - startAnalyzeTime} ***")
        case Some(masterActor) =>
          actor ! AnalyzeBlock(filePath, resultReceived * ParamAKKA.sizeOfaPartToAnalyze, ParamAKKA.sizeOfaPartToAnalyze, ParamAKKA.bufferSize, testToOperate)
      }
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }

  override def postStop(): Unit = {
    if(verbose) println(s"*** Stop Actor ${self.path} ***")
    super.postStop()
  }
}

private class BlockAnalyzer(verbose:Boolean) extends Actor {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate) =>

      val ID = startRead / length
      if (verbose) println(s"Start analyze of block $ID [$startRead - ${startRead + length}[ - Ref [${self.path}]")

      sender ! analyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate, verbose)

      if (verbose) println(s"Finished analyze of block $ID - Ref [${self.path}]")
    case RegisterMe(reg:ActorRef) => reg ! RegisterRootee(self)
    case badMessage => throw new IllegalArgumentException(s"Sent bad parameters (${badMessage.toString}) to Actor ${self.path}")
  }

  private def analyzeBlock(path: String, filePositionStartAnalyze: Long, lengthOfBlockToAnalyze: Long, bufferSize: Integer, testToOperate: Array[Byte] => Int, verbose: Boolean): Result = {
    val limitToAnalyze = filePositionStartAnalyze + lengthOfBlockToAnalyze
    val randomAccessFile = new RandomAccessFile(path, "r")
    val buffer = new Array[Byte](bufferSize)

    var searchResult: Option[(Int, Int)] = None
    try {
      randomAccessFile.seek(filePositionStartAnalyze)
      searchResult = Iterator
        .continually(randomAccessFile.read(buffer))
        .takeWhile(c => c != -1
        && randomAccessFile.getFilePointer <= limitToAnalyze + bufferSize) // stop when the end of file || block is reached
        .map(_ => buffer) // buffer
        .map(testToOperate)
        .zipWithIndex
        .find(_._1 != -1)

    } finally {
      randomAccessFile.close()
    }

    searchResult match {
      case None => Result(self, path, None, verbose)
      case Some((positionInArray: Int, arrayIndex: Int)) =>
        Result(self, path, Some(filePositionStartAnalyze + arrayIndex * ParamAKKA.bufferSize + positionInArray - 1), verbose) // remove 1 because first position in a file is zero.
      case _ => throw new IllegalStateException("Search result should be a Tuple of two Integers.")
    }
  }

  override def postStop(): Unit = {
    if (verbose) println(s"*** Rootee is dead ${self.path} ***")
    super.postStop()
  }
}

