package com.taj.unicode_detector.Encoding.FullCheck

import java.io.RandomAccessFile
import com.taj.unicode_detector.Encoding.FullCheck.FileFullAnalyzeStateMessages.{AnalyzeBlock, Result}
import akka.actor.{ActorContext, ActorRef, Props, Actor}
import com.typesafe.scalalogging.slf4j.Logging
import akka.routing.RoundRobinPool
import com.taj.unicode_detector.Encoding.BOM.BOMFileEncoding
import com.taj.unicode_detector.ParamAkka

/**
 * Created by geantvert on 19/03/14.
 */
private object BlockAnalyzer {
  def apply(nbrOfWorkers: Int, encodingTested: BOMFileEncoding)(implicit context: ActorContext): ActorRef = {
    context.actorOf(Props(new BlockAnalyzer()).withRouter(RoundRobinPool(nbrOfWorkers)), name = s"Router_${encodingTested.charsetUsed.name()}")
  }
}

/**
 * Actually process the encoding detection.
 */
private class BlockAnalyzer() extends Actor with Logging {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate) =>

      val analyzedBlockResult: FileFullAnalyzeStateMessages.Result = analyzeBlock(bigDataFilePath, startRead, length, buffer, testToOperate)

      sender ! analyzedBlockResult

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