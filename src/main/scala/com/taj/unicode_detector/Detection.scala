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
import com.taj.unicode_detector.ActorLifeOverview.StartRegistration
import scala.Some
import com.taj.unicode_detector.ActorLifeOverview.KillAkka
import com.taj.unicode_detector.TestResult.{ResultOfTestBOM, InitAnalyzeFile}
import com.typesafe.scalalogging.slf4j.Logging


object TestResult {

  case class InitAnalyzeFile()

  case class ResultOfTestBOM(result: Option[BOMFileEncoding])

  case class ResultOfTestFullFileAnalyze(category: BOMFileEncoding, nonMatchingBytePositionInFile: Option[Long], timeElapsed: Long, reaper: ActorRef)

}

class Detection(file: String) extends Actor {

  import BOMEncoding._
  import TestResult._

  var mActorUTF8: Option[ActorRef] = None
  var mFile: Option[String] = None

  var mOriginalSender: Option[ActorRef] = None

  def receive = {
    case InitAnalyzeFile() =>
      mFile = Some(file)
      mOriginalSender = Some(sender)
      val BOMActor = context.system.actorOf(Props(new BOMBasedDetectionActor(file)), name = "BOMActor")
      BOMActor ! InitAnalyzeFile()
    case ResultOfTestBOM(Some(detectedEncoding)) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(detectedEncoding))
    case ResultOfTestBOM(None) =>
      val asciiReaper = context.system.actorOf(Props(new Reaper()), "ASCIIReaper")
      val mActorASCIIActorRef = context.system.actorOf(Props(new FileAnalyzer(ASCII, mFile.get, ParamAkka.checkASCII)), name = "ASCIIFileAnalyzer")
      mActorASCIIActorRef ! StartRegistration(asciiReaper)
      mActorASCIIActorRef ! InitAnalyzeFile()
    case ResultOfTestFullFileAnalyze(ASCII, None, _, reaper) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(ASCII))
      reaper ! KillAkka()
    case ResultOfTestFullFileAnalyze(ASCII, Some(position), time, _) =>
      val UTF8Reaper = context.system.actorOf(Props(new Reaper()), "UTF8Reaper")
      val mActorUTF8ActorRef = context.system.actorOf(Props(new FileAnalyzer(UTF8NoBOM, mFile.get, ParamAkka.checkUTF8)), name = "UTF8FileAnalyzer")
      mActorUTF8ActorRef ! StartRegistration(UTF8Reaper)
      mActorUTF8ActorRef ! InitAnalyzeFile()
    case ResultOfTestFullFileAnalyze(UTF8NoBOM, None, time, reaper) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(UTF8NoBOM))
      reaper ! KillAkka()
    case ResultOfTestFullFileAnalyze(UTF8NoBOM, Some(position), time, reaper) =>
      mOriginalSender.get ! ResultOfTestBOM(Some(UnknownEncoding))
      reaper ! KillAkka()
  }
}

/**
 * First try to detect on the BOM then on the content.
 * @param file path to the file to test.
 */
class MiniDetection(file: String) extends Actor {

  import TestResult._

  var mActorUTF8: Option[ActorRef] = None
  var mFile: Option[String] = None

  var mOriginalSender: Option[ActorRef] = None

  def receive = {
    case InitAnalyzeFile() =>
      mFile = Some(file)
      mOriginalSender = Some(sender)
      val BOMActor = context.system.actorOf(Props(new BOMBasedDetectionActor(file)), name = "BOMActor")
      BOMActor ! InitAnalyzeFile()
    case ResultOfTestBOM(Some(detectedEncoding)) =>
      mOriginalSender.get ! detectedEncoding.charsetUsed

    case ResultOfTestBOM(None) =>
      mOriginalSender.get ! Converter.detectEncoding(file)

  }
}

//class DetectionResult() extends Actor {
//  override def receive: Actor.Receive = {
//    case ResultOfTestBOM(Some(detectedEncoding)) => detectedEncoding
//    case _ => throw new IllegalArgumentException("Failed to retrieve result from Actor during the check")
//  }
//}

/**
 * Main class to detect a file encoding based on its BOM.
 */
object Operations extends Logging {

  def detect(file: String): Charset = {
    implicit val timeout = Timeout(2, TimeUnit.MINUTES)

    val system: ActorSystem = ActorSystem("ActorSystemFileIdentification")
    val detector = system.actorOf(Props(new Detection(file)), name = "Detector")
    Await.result(detector ? InitAnalyzeFile(), timeout.duration) match {
      case ResultOfTestBOM(Some(detectedEncoding)) => detectedEncoding.charsetUsed.get
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
    val system: ActorSystem = ActorSystem("SystemEncodingFileIdentification")
    val detector = system.actorOf(Props(new MiniDetection(file)), name = "MiniDetector")
    val result = Await.result(detector ? InitAnalyzeFile(), timeout.duration) match {
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
   * @param charset the encoding of the file.
   * @param path the path to the file to the file.
   * @return
   */
  def removeBOM(charset: Charset, path: String): FileInputStream = {
    BOMEncoding.getBOMfromCharset(charset) match {
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
    val bomFrom = detect(from)
    val fileTo = new File(to)
    if (fileTo.exists()) fileTo.delete()
    if (fileTo.exists()) throw new IllegalStateException(s"File $to can't be deleted.")
    val output = new FileOutputStream(fileTo)

    val input = removeBOM(bomFrom, from)
    val bytes = new Array[Byte](1024) //1024 bytes - Buffer size
    try {
      Iterator
        .continually(input.read(bytes))
        .takeWhile(-1 !=)
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
        val input = removeBOM(detect(path), path)
        try Iterator
          .continually(input.read(bytes))
          .takeWhile(-1 !=)
          .foreach(read => try output.write(bytes, 0, read))
        finally input.close()
    } finally output.close()
  }
}