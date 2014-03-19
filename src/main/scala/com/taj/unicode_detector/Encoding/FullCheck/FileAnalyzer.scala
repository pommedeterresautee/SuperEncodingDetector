package com.taj.unicode_detector.Encoding.FullCheck

import com.taj.unicode_detector.Encoding.BOM.BOMFileEncoding
import akka.actor._
import com.typesafe.scalalogging.slf4j.Logging
import java.io.File
import com.taj.unicode_detector.ParamAkka
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.ActorLife.RegisterMe
import com.taj.unicode_detector.ActorLife.StartRegistration
import scala.Some
import akka.routing.Broadcast
import com.taj.unicode_detector.Encoding.FullCheck.FileFullAnalyzeStateMessages.AnalyzeBlock
import com.taj.unicode_detector.Encoding.FullCheck.FileFullAnalyzeStateMessages.Result
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze

/**
 * Created by geantvert on 19/03/14.
 */
object FileAnalyzer {
  def apply(encodingTested: BOMFileEncoding, path: String, testToOperate: Array[Byte] => Int, name: String)(implicit context: ActorContext): ActorRef = {
    context.system.actorOf(Props(new FileAnalyzer(encodingTested, path, testToOperate)), name = name)
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
  val routerBlockAnalyzerActor: ActorRef = BlockAnalyzer(nbrOfWorkers, encodingTested)

  var masterSender: Option[ActorRef] = None
  //  var mReaper: Option[ActorRef] = None
  var startAnalyzeTime = 0l
  var resultReceived = 0

  val discardAkkaMessages: Receive = {
    case _ => // do nothing
  }

  val resultState: Receive = {
    case Result(actor, filePath, nonMatchingCharPositionInFile) if resultReceived == numberOfPartToAnalyze && nonMatchingCharPositionInFile.isEmpty => // finished and match
      context.become(discardAkkaMessages)
      masterSender.get ! ResultOfTestBOM(Some(encodingTested))
      routerBlockAnalyzerActor ! Broadcast(PoisonPill)
    case Result(actor, filePath, nonMatchingCharPositionInFile) if nonMatchingCharPositionInFile.isDefined => // finished no match
      context.become(discardAkkaMessages)
      masterSender.get ! ResultOfTestBOM(None)
      routerBlockAnalyzerActor ! Broadcast(PoisonPill)
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
      register ! RegisterMe(routerBlockAnalyzerActor)
    case StartFileAnalyze() =>
      startAnalyzeTime = System.currentTimeMillis
      masterSender = Some(sender())
      context.become(resultState)

      // Initialization of the workers
      (0 to nbrOfWorkers - 1)
        .foreach(partNumber =>
        routerBlockAnalyzerActor ! AnalyzeBlock(path, partNumber * ParamAkka.sizeOfaPartToAnalyze, ParamAkka.sizeOfaPartToAnalyze, ParamAkka.bufferSize, testToOperate))

    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }

  override def postStop(): Unit = {
    logger.debug(s"*** Processed Actor ${self.path} in ${System.currentTimeMillis() - startAnalyzeTime}ms ***")
  }
}