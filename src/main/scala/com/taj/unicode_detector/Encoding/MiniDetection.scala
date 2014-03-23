package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import akka.actor._
import com.taj.unicode_detector.Encoding.BOM.BOMBasedDetectionActor
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.Reaper

import java.nio.charset.Charset
import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.ActorLife.{RegisterMe, KillAkka}
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some
import java.io.File

object MiniDetection extends Logging {
  def apply(path: String, output: Option[String])(implicit system: ActorSystem): ActorRef = new RealMiniDetectionTrait(path, output).actorRefResult
}

object MiniDetectionTest extends Logging {
  def apply(path: String, testActor: ActorRef)(implicit system: ActorSystem): ActorRef = {
    val fileName = new File(path).getName
    val actor = new MiniDetectionActorComponent() with ResultMiniDetectionTrait {
      val reaper = Reaper(s"MiniDetectionReaper_$fileName")
      override val detector = system.actorOf(Props(new MiniDetection(path)), s"MiniDetection_$fileName")
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
  class MiniDetection(file: String) extends Actor {
    lazy val HeuristicActor = HeuristicEncodingDetection(file)(context.system)

    def BOMDetectionResultReceive: Receive = {
      case ResultOfTestBOM(Some(detectedEncoding)) =>
        sender ! PoisonPill
        actorRefResult ! detectedEncoding.charsetUsed
      case ResultOfTestBOM(None) =>
        sender ! PoisonPill
        reaper ! RegisterMe(HeuristicActor)
        HeuristicActor ! StartFileAnalyze()
    }

    def receive = {
      case StartFileAnalyze() =>
        context.become(BOMDetectionResultReceive)
        val BOMActor = BOMBasedDetectionActor(file)
        reaper ! RegisterMe(BOMActor)
        reaper ! RegisterMe(actorRefResult)
        BOMActor ! StartFileAnalyze()
    }
  }

}

class RealMiniDetectionTrait(path: String, output: Option[String])(implicit system: ActorSystem) extends MiniDetectionActorComponent with ResultMiniDetectionTrait {
  override lazy val reaper = Reaper("MiniDetectionReaper")
  override lazy val detector = system.actorOf(Props(new MiniDetection(path)), "MiniDetection")
  override lazy val actorRefResult: ActorRef = SendBackActor(detector, reaper)

}

trait ResultMiniDetectionTrait {
  val actorRefResult: ActorRef
  val detector: ActorRef
  val reaper: ActorRef
}

object SendBackActor {
  def apply(firstActor: ActorRef, reaper: ActorRef)(implicit system: ActorSystem) = system.actorOf(Props(new SendBackActor(firstActor, reaper)), "SendBackActor")
}

class SendBackActor(miniDetector: ActorRef, reaper: ActorRef) extends Actor {

  var originalSender: Option[ActorRef] = None

  override def receive: Actor.Receive = {
    case StartFileAnalyze() =>
      originalSender = Some(sender())
      miniDetector ! StartFileAnalyze()
    case charset: Charset =>
      originalSender.get ! charset
      reaper ! KillAkka()
  }
}