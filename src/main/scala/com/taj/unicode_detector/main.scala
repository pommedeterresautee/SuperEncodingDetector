package com.taj.unicode_detector

import java.io.File
import akka.actor.{Props, ActorSystem}

object main extends App {


  val pathToFile = "C:\\Users\\MBenesty\\Private\\GIT\\unicode_detector\\FEC_EXAMPLE\\test.txt"
  val percentageToAnalyze = 100

  val bytesToRead = new File(pathToFile).length() * percentageToAnalyze / 100

  // Determine the minimum of Workers depending of the size of the file and the size of the buffer.
  // If we are working on a small file, start less workers, if it s a big file, use the number of cores.
  val workerCount = (1 to Runtime.getRuntime.availableProcessors).find(_ * ParamAkka.bufferSize >= bytesToRead).getOrElse(Runtime.getRuntime.availableProcessors)

  val system = ActorSystem("AsciiDetector")

  val logger = system.actorOf(Props[TheLogger], name = "LoggerWorker")
  val master = system.actorOf(Props(new FileAnalyzer(logger, workerCount, bytesToRead)), name = "FileAnalyzer")

  master ! AnalyzeFile(pathToFile)
}

/*
* isASCII = Iterator
        .continually(randomAccessFile.read(buffer))
        .takeWhile(c => c != -1
        && randomAccessFile.getFilePointer <= limitToAnalyze + main.bufferSize) // stop when the end of file || block is reached
        .zipWithIndex
        .flatMap{case(_, bufferCounter) => buffer.map((_, bufferCounter))}
        .zipWithIndex
        .map{case((byte, bufferCounter), arrayCounter) => (byte, bufferCounter, arrayCounter)}
        .find{case(testedByte, bufferCounter, arrayCounter) => testedByte < 0 && testedByte > 127}
* */