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
case class Calculate(path:String) extends AkkaMessage
case class Work(start: Int, filePath: String, startRead: Long, length: Int) extends AkkaMessage
case class Result(value: Boolean) extends AkkaMessage


class FileWorker extends Actor {

  def receive = {
    case Work(start, bigDataFilePath, startRead, length) =>
      println("Enter in Worker")
      sender ! analyzeBlock(bigDataFilePath, startRead, length)
  }

  def analyzeBlock(path: String, startRead: Long, length: Int):Result = {
    val randomAccessFile = new RandomAccessFile(path, "r")
    var isASCII:Boolean = false
    try{
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

class Master(nrOfWorkers: Int)
  extends Actor {
  val start: Long = System.currentTimeMillis
  val workerRouter = context.actorOf(
    Props[FileWorker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")

  def receive = {
    case Calculate(path) =>
      println("Enter in Master")
      workerRouter ! Work(0, path, 0, 1000)
    case Result(isASCII) => println(if (isASCII) "ascii" else "non ascii")
      context.stop(self)
  }
}

object main extends App {

  val pathToFEC = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\FEC.TXT"
  val percentage = 10

  val bytesToRead = if(Int.MaxValue < new File(pathToFEC).length() * percentage / 100) {
    println(s"The analyze of the file will be limited to ${Int.MaxValue / (1024 * 1024)}Mb")
    Int.MaxValue
  } else new File(pathToFEC).length() * percentage / 100

  val workerCount = Runtime.getRuntime.availableProcessors

  val system = ActorSystem("ASCIIdetector")

  val master = system.actorOf(Props(new Master(workerCount)), name = "master")

  // start the calculation
  master ! Calculate(pathToFEC)
}
