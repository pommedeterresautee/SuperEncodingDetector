package com.taj.unicode_detector.Encoding.BOM

import akka.actor.{Actor, Props, ActorRef, ActorSystem}
import com.taj.unicode_detector.ActorLife.RegisterRootee
import com.taj.unicode_detector.ActorLife.StartRegistration
import com.taj.unicode_detector.Encoding.MessageResult

/**
 * Created by geantvert on 19/03/14.
 */
object BOMBasedDetectionActor {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new BOMBasedDetectionActor(path)), "BOMActor")
  }
}

/**
 * This class detects the encoding of a file based on its BOM.
 */
class BOMBasedDetectionActor(file: String) extends Actor {

  import BOMEncoding._
  import MessageResult._

  def receive = {
    case StartRegistration(register) =>
      register ! RegisterRootee(self)
    case StartFileAnalyze() =>
      val result = detectBOM(file)
      sender ! ResultOfTestBOM(result)
    case _ => throw new IllegalArgumentException(s"Failed to retrieve result from ${self.path} during BOM detection")
  }
}