package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import com.taj.unicode_detector.Encoding.MessageResult.{ResultOfTestBOM, StartFileAnalyze}
import akka.actor.{Props, ActorSystem, Actor, ActorRef}
import com.taj.unicode_detector.Encoding.BOM.BOMBasedDetectionActor
import com.typesafe.scalalogging.slf4j.Logging

/**
 * First try to detect on the BOM then on the content.
 * @param file path to the file to test.
 */
class MiniDetection(file: String) extends Actor {

  implicit val sys = context.system
  var mOriginalSender: Option[ActorRef] = None

  def BOMDetectionResultReceive: Receive = {
    case ResultOfTestBOM(Some(detectedEncoding)) =>
      mOriginalSender.get ! detectedEncoding.charsetUsed
    case ResultOfTestBOM(None) =>
      val HeuristicActor = HeuristicEncodingDetection(file)
      HeuristicActor ! StartFileAnalyze()
  }

  def receive = {
    case StartFileAnalyze() =>
      context.become(BOMDetectionResultReceive)
      mOriginalSender = Some(sender())
      val BOMActor = BOMBasedDetectionActor(file)
      BOMActor ! StartFileAnalyze()
  }
}

object MiniDetection extends Logging {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new MiniDetection(path)), "MiniDetection")
  }
}