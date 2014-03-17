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
import akka.routing.{RoundRobinPool, Broadcast}
import scala.Option
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.ActorLife.RegisterMe
import com.taj.unicode_detector.ActorLife.RegisterRootee
import com.taj.unicode_detector.ActorLife.StartRegistration

import scala.Some
import com.taj.unicode_detector.FileFullAnalyzeStateMessages.AnalyzeBlock
import com.taj.unicode_detector.TestResult.InitAnalyzeFile
import com.taj.unicode_detector.FileFullAnalyzeStateMessages.Result
import com.taj.unicode_detector.TestResult.ResultOfTestFullFileAnalyze

private object FileFullAnalyzeStateMessages {

  case class AnalyzeBlock(filePath: String, startRead: Long, length: Long, bufferSize: Int, testToOperate: Array[Byte] => Int)

  case class Result(actor: ActorRef, pathOfTheFileAnalyzed: String, nonMatchingCharPositionInFile: Option[Long])

}

object FileAnalyzer {
  def apply(encodingTested: BOMFileEncoding, path: String, testToOperate: Array[Byte] => Int, name: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new FileAnalyzer(encodingTested, path, testToOperate)), name = name)
  }
}

/**
 * Manage the detection.
 * @param encodingTested encoding to test.
 * @param path to the file to analyze.
 * @param testToOperate test to apply to each byte to valid an encoding.
 */
class FileAnalyzer(encodingTested: BOMFileEncoding, path: String, testToOperate: Array[Byte] => Int) extends Actor with Logging {
  val totalLengthToAnalyze = new File(path).length()
  val numberOfPartToAnalyze = (totalLengthToAnalyze / ParamAkka.sizeOfaPartToAnalyze).toInt match {
    case 0 => 1
    case count: Int => count
  }
  val nbrOfWorkers = ParamAkka.numberOfWorkerRequired(totalLengthToAnalyze)
  val routerBlockAnalyzer: ActorRef = BlockAnalyzer(nbrOfWorkers, encodingTested)

  var masterSender: Option[ActorRef] = None
  var mReaper: Option[ActorRef] = None
  var startAnalyzeTime = 0l
  var resultReceived = 0

  val discardAkkaMessages: Receive = {
    case _ => // do nothing
  }

  val resultState: Receive = {
    case Result(actor, filePath, nonMatchingCharPositionInFile) if resultReceived == numberOfPartToAnalyze && nonMatchingCharPositionInFile.isEmpty => // finished and match
      context.become(discardAkkaMessages)
      masterSender.get ! ResultOfTestFullFileAnalyze(Some(encodingTested), mReaper.get)
      routerBlockAnalyzer ! Broadcast(PoisonPill)
    case Result(actor, filePath, nonMatchingCharPositionInFile) if nonMatchingCharPositionInFile.isDefined => // finished no match
      context.become(discardAkkaMessages)
      masterSender.get ! ResultOfTestFullFileAnalyze(None, mReaper.get)
      routerBlockAnalyzer ! Broadcast(PoisonPill)
      logger.debug(s"First char non matching with the encoding ${encodingTested.charsetUsed.name()} is at position ${nonMatchingCharPositionInFile.get}.")
    case Result(actor, filePath, nonMatchingCharPositionInFile) => // continue process
      resultReceived += 1
      actor ! AnalyzeBlock(filePath, resultReceived * ParamAkka.sizeOfaPartToAnalyze, ParamAkka.sizeOfaPartToAnalyze, ParamAkka.bufferSize, testToOperate)
    case _ => throw new IllegalArgumentException(s"Sent bad parameters from ${sender().path} to ${self.path}")
  }

  override def preStart(): Unit = {
    logger.debug(
      s"""Start processing @$startAnalyzeTime.
        Current actor [${self.path}].
        Will use $nbrOfWorkers Workers."""
    )
  }

  def receive = {
    case StartRegistration(register) =>
      mReaper = Some(register)
      //      mReaper.get ! RegisterRooter(routerBlockAnalyzer)
      routerBlockAnalyzer ! Broadcast(RegisterMe(mReaper.get))

    case InitAnalyzeFile() =>
      startAnalyzeTime = System.currentTimeMillis
      masterSender = Some(sender())
      context.become(resultState)

      // Initialization of the workers
      (0 to nbrOfWorkers - 1)
        .foreach(partNumber =>
        routerBlockAnalyzer ! AnalyzeBlock(path, partNumber * ParamAkka.sizeOfaPartToAnalyze, ParamAkka.sizeOfaPartToAnalyze, ParamAkka.bufferSize, testToOperate))

    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }

  override def postStop(): Unit = {
    logger.debug(s"*** Processed Actor ${self.path} in ${System.currentTimeMillis() - startAnalyzeTime}ms ***")
  }
}

private object BlockAnalyzer {
  def apply(nbrOfWorkers: Int, encodingTested: BOMFileEncoding)(implicit context: ActorContext): ActorRef = {
    context.actorOf(Props(new BlockAnalyzer()).withRouter(RoundRobinPool(nbrOfWorkers)), name = s"Router_${encodingTested.charsetUsed.name()}")
  }
}

/**
 * Make the effective detection.
 */
private class BlockAnalyzer() extends Actor with Logging {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate) =>

      val analyzedBlockResult: FileFullAnalyzeStateMessages.Result = analyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate)

      sender ! analyzedBlockResult

    case RegisterMe(reg: ActorRef) => reg ! RegisterRootee(self)
    case badMessage => throw new IllegalArgumentException(s"Sent bad parameters (${badMessage.toString}) to Actor ${self.path}")
  }

  private def analyzeBlock(path: String, filePositionStartAnalyze: Long, lengthOfBlockToAnalyze: Long, bufferSize: Integer, testToOperate: Array[Byte] => Int): Result = {
    val ID = filePositionStartAnalyze / lengthOfBlockToAnalyze
    logger.debug(s"Start analyze of block $ID [$filePositionStartAnalyze - ${filePositionStartAnalyze + lengthOfBlockToAnalyze}[ - Ref [${self.path}]")

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
      logger.debug(s"Finished analyze of block $ID - Ref [${self.path}]")
    }

    searchResult match {
      case None => Result(self, path, None)
      case Some((positionInArray: Int, arrayIndex: Int)) =>
        Result(self, path, Some(filePositionStartAnalyze + arrayIndex * ParamAkka.bufferSize + positionInArray - 1)) // remove 1 because first position in a file is zero.
      case _ => throw new IllegalStateException("Search result should be a Tuple of two Integers.")
    }
  }
}