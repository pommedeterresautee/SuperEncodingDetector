/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014. TAJ - Société d'avocats
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * EXCEPT AS CONTAINED IN THIS NOTICE, THE NAME OF TAJ - Société d'avocats SHALL
 * NOT BE USED IN ADVERTISING OR OTHERWISE TO PROMOTE THE SALE, USE OR OTHER
 * DEALINGS IN THIS SOFTWARE WITHOUT PRIOR WRITTEN AUTHORIZATION FROM
 * TAJ - Société d'avocats.
 */

package com.taj.unicode_detector.Encoding

import com.taj.unicode_detector.Encoding.Heuristic.HeuristicEncodingDetection
import akka.actor._
import com.taj.unicode_detector.Encoding.BOM.BOMBasedDetectionActor
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.Reaper

import com.taj.unicode_detector.Encoding.MessageResult.ResultOfTestBOM
import com.taj.unicode_detector.ActorLife.RegisterMe
import com.taj.unicode_detector.Encoding.MessageResult.StartFileAnalyze
import scala.Some
import java.io.File

trait ResultMiniDetectionProvider {
  val detector: ActorRef
  val reaper: ActorRef
  val actorRefResult: ActorRef
}

object BackMiniDetectionProduction extends Logging {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = new BackMiniDetectionProvider(path).actorRefResult
}

class BackMiniDetectionProvider(path: String)(implicit system: ActorSystem) extends MiniDetectionActorComponent with ResultMiniDetectionProvider {
  override lazy val reaper = Reaper("MiniDetectionReaper")
  override lazy val detector = MiniDetection(path, "")
  override lazy val actorRefResult = SendBackActor(detector, reaper)
}

object RealMiniDetectionProduction extends Logging {
  def apply(path: String, output: Option[String])(implicit system: ActorSystem): ActorRef = new RealMiniDetectionProvider(path, output).detector
}

class RealMiniDetectionProvider(path: String, output: Option[String])(implicit system: ActorSystem) extends MiniDetectionActorComponent with ResultMiniDetectionProvider {
  override lazy val reaper = Reaper("MiniDetectionReaper", autoStopWhenWorkerFinished = true)
  override lazy val detector = MiniDetection(path, "")
  override lazy val actorRefResult = EncodingResultActor(path, output)
}

object MiniDetectionTest extends Logging {
  def apply(path: String, testActor: ActorRef)(implicit system: ActorSystem): ActorRef = {
    val fileName = new File(path).getName
    val actor = new MiniDetectionActorComponent() with ResultMiniDetectionProvider {
      val reaper = Reaper(s"MiniDetectionReaper_$fileName")
      override val detector = MiniDetection(path, fileName)
      override val actorRefResult: ActorRef = testActor
    }
    actor.detector
  }
}

trait MiniDetectionActorComponent {
  self: ResultMiniDetectionProvider ⇒

  object MiniDetection {
    def apply(path: String, fileName: String)(implicit system: ActorSystem) = system.actorOf(Props(new MiniDetection(path)), s"MiniDetection_$fileName")
  }

  /**
   * First try to detect on the BOM then on the content.
   * @param file path to the file to test.
   */
  class MiniDetection(file: String) extends Actor with Logging {
    lazy val HeuristicActor = HeuristicEncodingDetection(file)(context.system)

    def BOMDetectionResultReceive: Receive = {
      case ResultOfTestBOM(Some(detectedEncoding)) ⇒
        sender ! PoisonPill
        actorRefResult ! detectedEncoding.charsetUsed
      case ResultOfTestBOM(None) ⇒
        sender ! PoisonPill
        reaper ! RegisterMe(HeuristicActor)
        HeuristicActor ! StartFileAnalyze()
    }

    def receive = {
      case StartFileAnalyze() ⇒
        context.become(BOMDetectionResultReceive)
        val BOMActor = BOMBasedDetectionActor(file)
        reaper ! RegisterMe(BOMActor)
        reaper ! RegisterMe(actorRefResult)
        BOMActor ! StartFileAnalyze()
    }
  }
}