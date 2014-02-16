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

import java.io.RandomAccessFile

import akka.actor._
import akka.routing.RoundRobinRouter


sealed trait AkkaMessage

case class AnalyzeFile(path: String) extends AkkaMessage
case class AnalyzeBlock(filePath: String, startRead: Long, length: Long, bufferSize:Int) extends AkkaMessage
case class Result(value: Boolean) extends AkkaMessage
case class FinalFullCheckResult(isASCII: Boolean, timeElapsed: Long) extends AkkaMessage

object ParamAkka {
  val bufferSize:Int = 1024*1024*10
}

class TheLogger extends Actor {
  def receive = {
    case FinalFullCheckResult(isBlockASCII, time) ⇒
      println(s"the result of the analyze is ${if (isBlockASCII) "ascii" else "non ascii"} and has been obtained in "/*${time/1000}s"*/)
      context.system.shutdown() // stop all the actors
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }
}

class FileAnalyzer(logger:ActorRef, nbrOfWorkers: Int, totalLengthToAnalyze:Long) extends Actor {

  val startTime = System.currentTimeMillis // to compute time elapsed to give a result
  val router = context.actorOf(Props[BlockAnalyzer].withRouter(RoundRobinRouter(nbrOfWorkers)), name = "workerRouter")
  val lengthPerWorkerToAnalyze = totalLengthToAnalyze / nbrOfWorkers
  var resultOfAnalyze = true // init
  var resultReceived = 0 // init

  def receive = {
    case AnalyzeFile(path) =>
      println("Start the processing...")
      (0 to nbrOfWorkers - 1)
        .foreach(workerNbr => 
        router ! AnalyzeBlock(path, workerNbr * lengthPerWorkerToAnalyze, lengthPerWorkerToAnalyze, ParamAkka.bufferSize))
    case Result(isBlockASCII) => 
      resultReceived += 1
      resultOfAnalyze &= isBlockASCII
      if(resultReceived == nbrOfWorkers || !resultOfAnalyze){
        logger ! FinalFullCheckResult(isBlockASCII, System.currentTimeMillis() - startTime)
        context.stop(self) // stop this actor and its children
      }
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }
}

private class BlockAnalyzer extends Actor {

  def receive = {
    case AnalyzeBlock(bigDataFilePath, startRead, length, buffer) =>
      val ID = startRead / length
      println(s"Start analyze of block $ID [$startRead - ${startRead + length}[")
      sender ! analyzeBlock(bigDataFilePath, startRead, length, buffer)
      println(s"Stop analyze of block $ID")
    case _ => throw new IllegalArgumentException("Sent bad parameters to Actor " + self.path.name)
  }

  private def analyzeBlock(path: String, startRead: Long, lengthOfBlockToAnalyze: Long, bufferSize:Integer): Result = {
    val limitToAnalyze = startRead + lengthOfBlockToAnalyze
    val randomAccessFile = new RandomAccessFile(path, "r")
    val buffer = new Array[Byte](bufferSize)

    var isASCII = false
    try {
      randomAccessFile.seek(startRead)
      isASCII = Iterator
        .continually(randomAccessFile.read(buffer))
        .takeWhile(c => c != -1
        && randomAccessFile.getFilePointer <= limitToAnalyze + bufferSize) // stop when the end of file || block is reached
        .flatMap(_ => buffer)
        .map(_.toInt)
        .forall{testedByte => testedByte >= 0 && testedByte <= 127}
    } finally {
      randomAccessFile.close()
    }
    Result(isASCII)
  }
}

