package com.taj.unicode_detector.Encoding

import akka.actor._
import java.nio.charset.Charset
import java.io.{FileWriter, BufferedWriter, File}
import com.typesafe.scalalogging.slf4j.Logging
import scala.Some


object EncodingResult extends Logging {
  def apply(path: String, output: Option[String])(implicit context: ActorContext): ActorRef = {
    context.system.actorOf(Props(new EncodingResult(path, output)), "EncodingResult")
  }
}

/**
 * Manage the result of an encoding detection result
 */
class EncodingResult(path: String, output: Option[String]) extends Actor with Logging {
  override def receive: Receive = {
    case charset: Charset =>
      val result = s"${new File(path).getName}|$charset"
      output match {
        case None => println(result)
        case Some(outputPath) =>
          val w = new BufferedWriter(new FileWriter(outputPath, true))
          w.write(result)
          w.newLine()
          w.close()
      }
    case _ => throw new IllegalArgumentException("Wrong argument provided")
  }
}