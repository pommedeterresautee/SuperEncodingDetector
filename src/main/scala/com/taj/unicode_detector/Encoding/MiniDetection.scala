package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import akka.actor._
import com.taj.unicode_detector.Encoding.BOM.BOMBasedDetectionActor
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some

object MiniDetection extends Logging {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new MiniDetection(path)), "MiniDetection")
  }
}

/**
 * First try to detect on the BOM then on the content.
 * @param file path to the file to test.
 */
class MiniDetection(file: String) extends Actor {

  var mOriginalSender: Option[ActorRef] = None
  val actorResult = EncodingResult(file, None)

  def BOMDetectionResultReceive: Receive = {
    case ResultOfTestBOM(Some(detectedEncoding)) =>
      mOriginalSender.get ! detectedEncoding.charsetUsed
      actorResult ! detectedEncoding.charsetUsed
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