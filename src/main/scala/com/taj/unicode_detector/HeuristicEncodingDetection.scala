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

import java.nio.charset.Charset
import com.ibm.icu.text.CharsetDetector
import java.io.{BufferedInputStream, FileInputStream}
import akka.actor.{Props, ActorSystem, ActorRef, Actor}
import com.taj.unicode_detector.ActorLife.{RegisterRootee, StartRegistration}
import com.taj.unicode_detector.TestResult.{ResultOfTestBOM, StartFileAnalyze}
import com.typesafe.scalalogging.slf4j.Logging


object HeuristicEncodingDetection extends Logging {
  def apply(path: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new HeuristicEncodingDetection(path)), "HeuristicEncodingDetection")
  }

  /**
   * Detects the encoding of a text file based on Heuristic analyze.
   * @param path path to the file to analyze.
   * @return the name of the encoding as a String.
   */
  def detectEncoding(path: String): Charset = {
    val detector = new CharsetDetector()
    val is = new BufferedInputStream(new FileInputStream(path))
    if (!is.markSupported()) throw new IllegalStateException(s"The file $path can't be analyzed because its InputStream doesn't support a reset.")
    try detector.setText(is)
    finally is.close()
    val matcher = detector.detectAll()
    logger.debug(s"detected encoding of file $path: ${matcher.map(_.getName).mkString(";")}")
    Charset.forName(matcher(0).getName)
  }
}

/**
 * Proceed to the encoding detection based on an Heuristic detection.
 * @param path to the file to analyze.
 */
class HeuristicEncodingDetection(path: String) extends Actor {
  def receive = {
    case StartRegistration(register) =>
      register ! RegisterRootee(self)
    case StartFileAnalyze() =>
      val encoding = HeuristicEncodingDetection.detectEncoding(path)
      val result: ResultOfTestBOM = ResultOfTestBOM(Some(BOMFileEncoding(encoding)))
      sender ! result
    case _ => throw new IllegalArgumentException(s"Failed to retrieve result from ${self.path} during BOM detection")
  }
}
