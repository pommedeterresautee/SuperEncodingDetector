package com.taj.unicode_detector.Encoding

import akka.actor.{ ActorRef, Actor }
import com.taj.unicode_detector.Encoding.BOM.{ BOMBasedDetectionActor, BOMFileEncoding, BOMEncoding }
import java.io.File
import com.taj.unicode_detector.{ ParamAkka, Reaper }
import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection._
import com.taj.unicode_detector.ActorLife.StartRegistration
import scala.Some
import com.taj.unicode_detector.ActorLife.KillAkka
import com.taj.unicode_detector.Encoding.MessageResult.{ ResultOfTestBOM, StartFileAnalyze }
import com.taj.unicode_detector.Encoding.FullCheck.FileAnalyzer

/**
 * Detect encoding of a file based on BOM detection then on full check algorithm.
 * @param filePath path to the file to analyze.
 */
class FullDetection(filePath: String) extends Actor {

  import BOMEncoding._

  var mActorUTF8: Option[ActorRef] = None
  var mFile: Option[String] = None
  val fileName = new File(filePath).getName
  val reaper = Reaper(s"Reaper_$fileName")(context.system)

  var mOriginalSender: Option[ActorRef] = None

  def bomResultReceive: Receive = {
    case ResultOfTestBOM(Some(detectedEncoding)) ⇒
      mOriginalSender.get ! ResultOfTestBOM(Some(detectedEncoding))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) ⇒
      context.become(ASCIIResultReceive)
      val mActorASCIIActorRef = FileAnalyzer(ASCII, mFile.get, ParamAkka.checkASCII, name = "ASCIIFileAnalyzer")
      mActorASCIIActorRef ! StartRegistration(reaper)
      mActorASCIIActorRef ! StartFileAnalyze()
  }

  def ASCIIResultReceive: Receive = {
    case ResultOfTestBOM(Some(ASCII)) ⇒
      mOriginalSender.get ! ResultOfTestBOM(Some(ASCII))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) ⇒
      context.become(UTF8ResultReceive)
      val mActorUTF8ActorRef = FileAnalyzer(UTF8NoBOM, mFile.get, ParamAkka.checkUTF8, name = "UTF8FileAnalyzer")
      mActorUTF8ActorRef ! StartRegistration(reaper)
      mActorUTF8ActorRef ! StartFileAnalyze()
  }

  def UTF8ResultReceive: Receive = {
    case ResultOfTestBOM(Some(UTF8NoBOM)) ⇒
      mOriginalSender.get ! ResultOfTestBOM(Some(UTF8NoBOM))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) ⇒
      mOriginalSender.get ! ResultOfTestBOM(Some(BOMFileEncoding(detectEncoding(filePath))))
      reaper ! KillAkka()
  }

  def receive = {
    case StartFileAnalyze() ⇒
      context.become(bomResultReceive)
      mFile = Some(filePath)
      mOriginalSender = Some(sender())
      val BOMActor = BOMBasedDetectionActor(filePath)
      BOMActor ! StartFileAnalyze()
  }
}
