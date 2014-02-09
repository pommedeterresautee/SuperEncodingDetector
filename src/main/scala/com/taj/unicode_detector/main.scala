/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 TAJ - Société d'avocats
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
 */

package com.taj.unicode_detector

import java.io.{RandomAccessFile, File}

import akka.actor._
import akka.routing.RoundRobinRouter


sealed trait AkkaMessage

case class AnalyzeFile(path: String) extends AkkaMessage
case class AnalyzeBlock(filePath: String, startRead: Long, length: Int) extends AkkaMessage
case class Result(value: Boolean) extends AkkaMessage
case class ToPrint(textResult: String, timeElapsed:Long) extends AkkaMessage

class TheLogger extends Actor {
  def receive = {
    case ToPrint(text, time) ⇒
      println(s"the result of the analyze is $text and has been obtained in ${time/1000}s")
      context.system.shutdown()
  }
}

class FileAnalyzer(logger:ActorRef, nrOfWorkers: Int, lengthFileToAnalyze:Int) extends Actor {
  val start = System.currentTimeMillis
  val workerRouter = context.actorOf(Props[BlockAnalyzer].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
  val lengthBlockToAnalyze = lengthFileToAnalyze / nrOfWorkers
  var analyzeResult = true
  var resultReceived = 0

  def receive = {
    case AnalyzeFile(path) =>
      println("Enter in Master")
      (0 to nrOfWorkers-1).foreach(workerNbr => workerRouter ! AnalyzeBlock(path, workerNbr * lengthBlockToAnalyze, lengthBlockToAnalyze))
    case Result(isBlockASCII) => resultReceived += 1
      analyzeResult &= isBlockASCII
      if(resultReceived == nrOfWorkers || !analyzeResult){
        logger ! ToPrint(if (isBlockASCII) "ascii" else "non ascii", System.currentTimeMillis() - start)
        context.stop(self)
      }
  }
}

class BlockAnalyzer extends Actor {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length) =>
      val ID = startRead / length
      println(s"Enter in Worker $ID @$startRead - ${startRead + length - 1}")
      sender ! analyzeBlock(bigDataFilePath, startRead, length)
      println(s"Quit Worker $ID")
  }

  def analyzeBlock(path: String, startRead: Long, length: Int): Result = {
    val randomAccessFile = new RandomAccessFile(path, "r")
    var isASCII = false
    try {
      isASCII = Iterator
        .continually(randomAccessFile.read())
        .takeWhile(c => c != -1)
        .take(length)
        .forall(Character.UnicodeBlock.of(_) == Character.UnicodeBlock.BASIC_LATIN)

    } finally {
      randomAccessFile.close()
    }
    Result(isASCII)
  }
}

object main extends App {
  val pathToFEC = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\FEC.TXT"
  val percentageToAnalyze = 20

  val bytesToRead:Int = if (new File(pathToFEC).length() * percentageToAnalyze / 100 >= Int.MaxValue) {
    println(s"The analyze of the file will be limited to ${Int.MaxValue / (1024 * 1024)}Mb")
    Int.MaxValue
  } else (new File(pathToFEC).length() * percentageToAnalyze / 100).toInt

  val workerCount = Runtime.getRuntime.availableProcessors

  val system = ActorSystem("AsciiDetector")

  val logger = system.actorOf(Props[TheLogger], name = "listener")
  val master = system.actorOf(Props(new FileAnalyzer(logger, workerCount, bytesToRead)), name = "FileAnalyzer")

  master ! AnalyzeFile(pathToFEC)
}