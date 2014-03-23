package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import akka.actor._
import com.taj.unicode_detector.Encoding.BOM.BOMBasedDetectionActor
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import com.taj.unicode_detector.Reaper
import com.taj.unicode_detector.ActorLife._
import scala.reflect.io.File
import scala.concurrent.Await
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some
import java.nio.charset.Charset
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.ActorLife.StartRegistration
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some
import com.taj.unicode_detector.ActorLife.KillAkka

object MiniDetection extends Logging {
  def apply(path: String, output: Option[String])(implicit system: ActorSystem): ActorRef = {
    val reaper = Reaper("MiniDetectionReaper")
    val actor = new MiniDetectionActorComponent() with ResultMiniDetectionTrait {
      override val detector = system.actorOf(Props(new MiniDetection(path, reaper)), "MiniDetection")
      override val actorRefResult: ActorRef = system.actorOf(Props(new SendBackActor(detector)), "sendBackActor")
      reaper ! RegisterMe(actorRefResult)

    }
    actor.actorRefResult
  }
}

object MiniDetectionTest extends Logging {
  def apply(path: String, testActor: ActorRef)(implicit system: ActorSystem): ActorRef = {
    val reaper = Reaper("MiniDetectionReaper")
    val actor = new MiniDetectionActorComponent() with ResultMiniDetectionTrait {
      override val detector = system.actorOf(Props(new MiniDetection(path, reaper)), "MiniDetection")
      override val actorRefResult: ActorRef = testActor
    }
    actor.detector
  }
}

trait MiniDetectionActorComponent {
  self: ResultMiniDetectionTrait =>

  /**
   * First try to detect on the BOM then on the content.
   * @param file path to the file to test.
   */
  class MiniDetection(file: String, reaper: ActorRef) extends Actor {

    def BOMDetectionResultReceive: Receive = {
      case ResultOfTestBOM(Some(detectedEncoding)) =>
        actorRefResult ! detectedEncoding.charsetUsed
        reaper ! KillAkka()
      case ResultOfTestBOM(None) =>
        val HeuristicActor = HeuristicEncodingDetection(file)
        HeuristicActor ! StartRegistration(reaper)
        HeuristicActor ! StartFileAnalyze()
    }

    def receive = {
      case StartFileAnalyze() =>
        context.become(BOMDetectionResultReceive)
        val BOMActor = BOMBasedDetectionActor(file)
        actorRefResult ! StartRegistration(reaper)
        BOMActor ! StartRegistration(reaper)
        BOMActor ! StartFileAnalyze()
    }
  }

}

trait ResultMiniDetectionTrait {
  val actorRefResult: ActorRef
  val detector: ActorRef
}

class SendBackActor(miniDetector: ActorRef) extends Actor {

  var originalSender: Option[ActorRef] = None

  override def receive: Actor.Receive = {
    case StartFileAnalyze() =>
      originalSender = Some(sender())
      miniDetector ! StartFileAnalyze()
    case charset: Charset =>
      originalSender.get ! charset
  }
}