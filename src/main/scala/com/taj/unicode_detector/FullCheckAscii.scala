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
import com.taj.unicode_detector.ActorLifeOverview._
import com.taj.unicode_detector.FileFullAnalyzeStateMessages._
import com.taj.unicode_detector.TestResult.{ResultOfTestFullFileAnalyze, InitAnalyzeFile}

private object FileFullAnalyzeStateMessages {
  case class AnalyzeBlock(filePath: String, startRead: Long, length: Long, bufferSize: Int, testToOperate: Array[Byte] => Int)
  case class Result(actor: ActorRef, pathOfTheFileAnalyzed: String, nonMatchingCharPositionInFile: Option[Long], verbose: Boolean)
}

class FileAnalyzer(encodingTested:BOMFileEncoding , verbose: Boolean, path: String, testToOperate: Array[Byte] => Int) extends Actor {
  val totalLengthToAnalyze = new File(path).length()
  val numberOfPartToAnalyze = (totalLengthToAnalyze / ParamAkka.sizeOfaPartToAnalyze).toInt match {
    case 0 => 1
    case count: Int => count
  }

  val nbrOfWorkers = ParamAkka.numberOfWorkerRequired(totalLengthToAnalyze)
  val routerBlockAnalyzer: ActorRef = context.actorOf(Props(new BlockAnalyzer(verbose)).withRouter(RoundRobinRouter(nbrOfWorkers)), name = s"Router_${encodingTested.charsetName}")
  
  var masterSender: Option[ActorRef] = None
  var mReaper:Option[ActorRef] = None
  var startAnalyzeTime = 0l
  var resultReceived = 0

  def receive = {
    case StartRegistration(register) =>
      mReaper = Some(register)
      mReaper.get ! RegisterRooter(routerBlockAnalyzer)
      routerBlockAnalyzer ! Broadcast(RegisterMe(mReaper.get))

    case InitAnalyzeFile() =>
    startAnalyzeTime = System.currentTimeMillis
      masterSender = Some(sender)

      if (verbose) println (
        s"""Start processing @$startAnalyzeTime
        Current actor [${self.path}]
        Received a message from ${sender.path}.
        Will use $nbrOfWorkers Workers."""
      )

      // Initialization of the workers
      (0 to nbrOfWorkers - 1)
        .foreach(partNumber =>
        routerBlockAnalyzer ! AnalyzeBlock(path, partNumber * ParamAkka.sizeOfaPartToAnalyze, ParamAkka.sizeOfaPartToAnalyze, ParamAkka.bufferSize, testToOperate))

      if (verbose) println(s"The sender is ${sender.path} but the parent is ${context.parent.path}")

    case Result(actor, filePath, nonMatchingCharPositionInFile, verboseActivated) =>
      resultReceived += 1

      masterSender match {
        case None =>
        case Some(masterActor) if resultReceived == numberOfPartToAnalyze || !nonMatchingCharPositionInFile.isEmpty =>
          masterActor ! ResultOfTestFullFileAnalyze(encodingTested, nonMatchingCharPositionInFile, System.currentTimeMillis() - startAnalyzeTime, mReaper.get)
          implicit val timeout = Timeout(2, TimeUnit.MINUTES)
          masterSender = None

          routerBlockAnalyzer ! Broadcast(PoisonPill)

          if (verboseActivated) println(s"*** Finished Actor ${self.path} process in ${System.currentTimeMillis() - startAnalyzeTime} ***")
        case Some(masterActor) =>
          actor ! AnalyzeBlock(filePath, resultReceived * ParamAkka.sizeOfaPartToAnalyze, ParamAkka.sizeOfaPartToAnalyze, ParamAkka.bufferSize, testToOperate)
      }
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
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
        Result(self, path, Some(filePositionStartAnalyze + arrayIndex * ParamAkka.bufferSize + positionInArray - 1), verbose) // remove 1 because first position in a file is zero.
      case _ => throw new IllegalStateException("Search result should be a Tuple of two Integers.")
    }
  }
}

