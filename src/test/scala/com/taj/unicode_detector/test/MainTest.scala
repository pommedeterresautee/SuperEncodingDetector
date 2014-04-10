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

package com.taj.unicode_detector.test

import org.scalatest._
import java.io.File

import com.typesafe.scalalogging.slf4j.Logger
import org.slf4j.LoggerFactory
import FirstListFilesToTest._
import SecondListFilesToTest._
import ThirdListFilesToTestWrongParameters._
import com.taj.unicode_detector.test.tests._

/**
 * Test the detection algorithm with each kind of file.
 */
class MainTest extends Suites(BOMTests, Conversion, DifferentBOM, EncodingTest, TestEncodingWithWrongParameter) with TestTrait with BeforeAndAfterAll {

  /**
   * Clean all temp files before starting
   */
  override def beforeAll() {
    val logger = Logger(LoggerFactory getLogger "Test logger")
    val tmpFolder = new File(tempFilesFolder)
    tmpFolder.listFiles().filter(!_.getName.contentEquals(".gitignore")).foreach(_.delete())
    tmpFolder.listFiles().length should be > 0
  }

  List(UTF8_with_BOM, UTF8_without_BOM, UTF16_BE, UTF16_LE, ASCII, Windows_1252, UTF8_with_BOM_bis, UTF8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis, UTF8_with_BOM_manually_cleaned, UTF16_BE_manually_cleaned, UTF16_LE_manually_cleaned)
    .foreach(EncodingTest.test)

  List(UTF8_with_BOM_error, UTF8_without_BOM_error, UTF16_BE_error, UTF16_LE_error, ASCII_error, Windows_1252_error)
    .foreach(TestEncodingWithWrongParameter.test)

  List((UTF8_with_BOM, UTF8_with_BOM_bis), (UTF8_without_BOM, UTF8_without_BOM_bis), (UTF16_BE, UTF16_BE_bis), (UTF16_LE, UTF16_LE_bis))
    .foreach(BOMTests.test)

  List((Windows_1252, ASCII), (UTF16_BE, UTF8_with_BOM), (UTF8_with_BOM, UTF16_LE), (UTF16_BE, UTF16_LE))
    .foreach(DifferentBOM.test)

  List((UTF8_with_BOM, UTF8_with_BOM_manually_cleaned), (UTF16_BE, UTF16_BE_manually_cleaned), (UTF16_LE, UTF16_LE_manually_cleaned), (ASCII, ASCII))
    .foreach(BOMTests.test2)

  List(ASCII, UTF8_with_BOM, UTF16_BE, UTF16_LE, UTF8_with_BOM_bis, UTF8_without_BOM_bis, UTF16_BE_bis, UTF16_LE_bis)
    .foreach(Conversion.test)

  List(UTF16_BE_bis, UTF16_LE_bis)
    .foreach(Conversion.test2)

  //TODO use this code to test command line
  val stream = new java.io.ByteArrayOutputStream()
  Console.withOut(stream) {
    println("test message")
  }
  stream.toString should include("test message")
}