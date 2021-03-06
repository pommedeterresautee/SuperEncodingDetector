package com.taj.unicode_detector.Encoding

import akka.actor._
import java.nio.charset.Charset
import java.io.{ FileWriter, BufferedWriter, File }
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.Some
import com.taj.unicode_detector.ActorLife.{ RegisterMe, StartRegistration }

object EncodingResultActor extends LazyLogging {
  def apply(path: String, output: Option[String])(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new EncodingResultActor(path, output)), "EncodingResult")
  }
}

/**
 * Manage the result of an encoding detection result
 */
class EncodingResultActor(path: String, output: Option[String]) extends Actor with LazyLogging {
  override def receive: Receive = {
    case StartRegistration(register) ⇒
      register ! RegisterMe(self)
    case charset: Charset ⇒
      val result = s"${new File(path).getName}|$charset"
      output match {
        case None ⇒ println(result)
        case Some(outputPath) ⇒
          val w = new BufferedWriter(new FileWriter(outputPath, true))
          w.write(result)
          w.newLine()
          w.close()
      }
      self ! PoisonPill
    case _ ⇒ throw new IllegalArgumentException("Wrong argument provided")
  }
}