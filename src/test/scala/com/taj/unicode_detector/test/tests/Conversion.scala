package com.taj.unicode_detector.test.tests

import com.taj.unicode_detector.test.FirstListFilesToTest._
import com.taj.unicode_detector.{BOMEncoding, Operations, Converter}
import java.io.File
import com.taj.unicode_detector.test.TestFile
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import com.typesafe.scalalogging.slf4j.Logging
import java.nio.charset.Charset

object Conversion extends WordSpecLike with Matchers with BeforeAndAfterAll with Logging {
  val test: TestFile => Unit = {
    file =>
      val fileToConvert = encodedFileFolder + file.fileName
      val convertedFile = tempFilesFolder + "converted_to_ASCII_" + file.fileName
      s"convert the file ${file.fileName} to ASCII" in {
        Converter.convert2ASCII(fileToConvert, convertedFile)
        new File(convertedFile) should be('exists)
        convertedFile.length should be > 0
        val encoding = Operations.detect(convertedFile)
        encoding should equal(BOMEncoding.ASCII.charsetUsed)
      }
  }

  val test2: TestFile => Unit = {
    fileToTest =>
      val file = new File(encodedFileFolder, fileToTest.fileName)

      s"convert the file ${file.getName} to UTF-8" in {
        val fileConverted = new File(tempFilesFolder, "converted_to_UTF-8_" + fileToTest.fileName)
        Converter.convert2UTF_8(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding = Operations.detect(fileConverted.getAbsolutePath)
        encoding should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
        val encoding_bis = Converter.detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(BOMEncoding.UTF8NoBOM.charsetUsed)
      }

      s"convert the file ${file.getName} to ISO 8859-15" in {
        val fileConverted = new File(tempFilesFolder, "converted_to_ISO_8859-15_" + fileToTest.fileName)
        Converter.convert2ISO_8859_15(file.getAbsolutePath, fileConverted.getAbsolutePath)
        fileConverted should be('exists)
        fileConverted.length should be > 0l
        val encoding_bis = Converter.detectEncoding(fileConverted.getAbsolutePath)
        encoding_bis should equal(Charset.forName("ISO-8859-1"))
      }
  }
}
