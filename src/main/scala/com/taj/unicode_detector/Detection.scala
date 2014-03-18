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

package com.taj.unicode_detector

import java.io.{File, FileOutputStream, FileInputStream}
import java.nio.file.{Paths, Files}
import java.nio.charset.Charset
import akka.actor._
import akka.util.Timeout
import scala.concurrent.Await
import akka.pattern.ask
import java.util.concurrent.TimeUnit
import com.taj.unicode_detector.ActorLife.StartRegistration
import scala.Some
import com.taj.unicode_detector.ActorLife.KillAkka
import com.taj.unicode_detector.TestResult.{ResultOfTestBOM, StartFileAnalyze}
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.HeuristicEncodingDetection._


object TestResult {

  case class StartFileAnalyze()

  case class ResultOfTestBOM(result: Option[BOMFileEncoding])

}

class Detection(filePath: String) extends Actor {

  import BOMEncoding._
  import TestResult._

  implicit val sys = context.system
  var mActorUTF8: Option[ActorRef] = None
  var mFile: Option[String] = None
  val fileName = new File(filePath).getName
  val reaper = Reaper(s"Reaper_$fileName")

  var mOriginalSender: Option[ActorRef] = None

  def bomResultReceive: Receive = {
    case ResultOfTestBOM(Some(detectedEncoding)) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(detectedEncoding))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) =>
      context.become(ASCIIResultReceive)
      val mActorASCIIActorRef = FileAnalyzer(ASCII, mFile.get, ParamAkka.checkASCII, name = "ASCIIFileAnalyzer")
      mActorASCIIActorRef ! StartRegistration(reaper)
      mActorASCIIActorRef ! StartFileAnalyze()
  }

  def ASCIIResultReceive: Receive = {
    case ResultOfTestBOM(Some(ASCII)) =>
    mOriginalSender.get ! ResultOfTestBOM(Some(ASCII))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) =>
      context.become(UTF8ResultReceive)
      val mActorUTF8ActorRef = FileAnalyzer(UTF8NoBOM, mFile.get, ParamAkka.checkUTF8, name = "UTF8FileAnalyzer")
      mActorUTF8ActorRef ! StartRegistration(reaper)
      mActorUTF8ActorRef ! StartFileAnalyze()
  }

  def UTF8ResultReceive: Receive = {
    case ResultOfTestBOM(Some(UTF8NoBOM)) =>
    mOriginalSender.get ! ResultOfTestBOM(Some(UTF8NoBOM))
      reaper ! KillAkka()
    case ResultOfTestBOM(None) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(BOMFileEncoding(detectEncoding(filePath), List(), List())))
      reaper ! KillAkka()
  }

  def receive = {
    case StartFileAnalyze() =>
      context.become(bomResultReceive)
      mFile = Some(filePath)
      mOriginalSender = Some(sender())
      val BOMActor = BOMBasedDetectionActor(filePath)
      BOMActor ! StartFileAnalyze()
  }
}


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

/**
 * Main class to detect a file encoding based on its BOM.
 */
object Operations extends Logging {

  def fullDetect(file: String): Charset = {
    implicit val timeout = Timeout(2, TimeUnit.MINUTES)

    val system: ActorSystem = ActorSystem("ActorSystemFileIdentification")
    val detector = system.actorOf(Props(new Detection(file)), name = "Detector")
    Await.result(detector ? StartFileAnalyze(), timeout.duration) match {
      case ResultOfTestBOM(Some(detectedEncoding)) => detectedEncoding.charsetUsed
      case _ => throw new IllegalArgumentException("Failed to retrieve result from Actor during the check")
    }
  }

  /**
   * Detects the encoding of a file based on their BOM or their content.
   * @param file path to the file to analyze.
   * @return the charset detected.
   */
  def miniDetect(file: String): Charset = {
    implicit val timeout = Timeout(2, TimeUnit.MINUTES)
    implicit val system: ActorSystem = ActorSystem("SystemMiniDetect")
    val detector = MiniDetection(file)
    val result = Await.result(detector ? StartFileAnalyze(), timeout.duration) match {
      case charset: Charset => charset
      case Some(charset: Charset) => charset
      case u => throw new IllegalArgumentException(s"Failed to retrieve result from Actor: $u.")
    }
    system.shutdown()
    result
  }

  /**
   * Compare encoding of several files.
   * @param verbose print some helpful messages.
   * @param paths paths to the files to compare.
   * @return true if the encoding is the same everywhere.
   */
  def isSameEncoding(verbose: Boolean, paths: String*): Boolean = {
    if (paths.size < 2) throw new IllegalArgumentException(s"Not enough files to compare (${paths.size})")
    val charset: Charset = miniDetect(paths.head)
    paths.tail.forall {
      path: String =>
        val detectedEncoding: Charset = miniDetect(path)
        val same = charset.equals(detectedEncoding)
        if (!same) logger.debug(s"The first file [${paths.head}] is encoded as ${charset.name()} but the file [$path] is encoded as ${detectedEncoding.name}.")
        same
    }
  }

  /**
   * Remove the bytes relative to the detected BOM of an existing text file.
   * @param path the path to the file to the file.
   * @return
   */
  def removeBOM(path: String): FileInputStream = {
    BOMEncoding.detect(path) match {
      case None => new FileInputStream(path)
      case Some(bom) =>
        val toDrop = bom.BOM.size
        val f = new FileInputStream(path)
        val realSkipped = f.skip(toDrop)
        if (toDrop != realSkipped) throw new IllegalStateException(s"Failed to skip the correct number of bytes for the file $path ($realSkipped instead of $toDrop)")
        f
    }
  }

  /**
   * Copy a file to another place without its BOM.
   * @param from path to the source.
   * @param to path to the destination.
   * @param verbose true to see more information about the process ongoing.
   */
  def copyWithoutBom(from: String, to: String, verbose: Boolean) {
    val fileTo = new File(to)
    if (fileTo.exists()) fileTo.delete()
    if (fileTo.exists()) throw new IllegalStateException(s"File $to can't be deleted.")
    val output = new FileOutputStream(fileTo)

    val input = removeBOM(from)
    val bytes = new Array[Byte](1024) //1024 bytes - Buffer size
    try {
      Iterator
        .continually(input.read(bytes))
        .takeWhile(_ != -1)
        .foreach(read => output.write(bytes, 0, read))
    } finally {
      input.close()
      output.close()
    }
  }

  /**
   * Merge several text files together even if they have a BOM.
   * @param destination path where to save the merged file.
   * @param paths paths to the files to merge together.
   */
  def mergeFilesWithoutBom(destination: String, paths: String*) {
    Files.copy(Paths.get(paths(0)), Paths.get(destination))
    val bytes = new Array[Byte](1024)
    val output = new FileOutputStream(destination, true)
    try paths.drop(1).foreach {
      path =>
        val input = removeBOM(path)
        try Iterator
          .continually(input.read(bytes))
          .takeWhile(_ != -1)
          .foreach(read => try output.write(bytes, 0, read))
        finally input.close()
    } finally output.close()
  }
}