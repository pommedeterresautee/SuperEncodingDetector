package com.taj.unicode_detector.Encoding

import akka.actor._
import java.nio.charset.Charset
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some
import com.taj.unicode_detector.ActorLife.KillAkka

class SendBackActor(miniDetector: ActorRef, reaper: ActorRef) extends Actor {
  var originalSender: Option[ActorRef] = None

  override def receive: Actor.Receive = {
    case StartFileAnalyze() ⇒
      originalSender = Some(sender())
      miniDetector ! StartFileAnalyze()
    case charset: Charset ⇒
      originalSender.get ! charset
      self ! PoisonPill
      reaper ! KillAkka()
  }
}

object SendBackActor {
  def apply(firstActor: ActorRef, reaper: ActorRef)(implicit system: ActorSystem) = system.actorOf(Props(new SendBackActor(firstActor, reaper)), "SendBackActor")
}