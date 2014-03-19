package com.taj.unicode_detector.test

import java.io.File
import java.nio.charset.Charset
import com.taj.unicode_detector.Encoding.BOM.{BOMEncoding, BOMFileEncoding}


/**
 * A case class to contain the parameters of a test file.
 * @param fileName the name of the test file.
 * @param encoding Encoding type of the file.
 */
case class TestFile(fileName: String, encoding: BOMFileEncoding, workingActorsNeeded: Int)

object FirstListFilesToTest {
  val testResourcesFolder = s".${File.separator}src${File.separator}test${File.separator}resources${File.separator}"
  val encodedFileFolder = testResourcesFolder + s"encoded_files${File.separator}"
  val tempFilesFolder = testResourcesFolder + s"temp${File.separator}"

  // First list of text files with or without BOM
  val UTF8_with_BOM = TestFile("UTF8_with_BOM.txt", BOMEncoding.UTF8, 1)
  val UTF8_without_BOM = TestFile("UTF8_without_BOM.txt", BOMEncoding.UTF8NoBOM, 1)
  val UTF16_BE = TestFile("UTF16_BE.txt", BOMEncoding.UTF_16_BE, 1)
  val UTF16_LE = TestFile("UTF16_LE.txt", BOMEncoding.UTF_16_LE, 1)
  val ASCII = TestFile("ASCII.txt", BOMEncoding.ASCII, 1)
  val Windows_1252 = TestFile("Windows_1252.txt", BOMFileEncoding(Charset.forName("ISO-8859-2")), 1)
}

/**
 * Second list of files with BOM for comparison purpose.
 */
object SecondListFilesToTest {
  val UTF8_with_BOM_bis = TestFile("UTF8_with_BOM_bis.txt", BOMEncoding.UTF8, 1)
  val UTF8_without_BOM_bis = TestFile("UTF8_without_BOM_bis.txt", BOMEncoding.UTF8NoBOM, 1)
  val UTF16_BE_bis = TestFile("UTF16_BE_bis.txt", BOMEncoding.UTF_16_BE, 1)
  val UTF16_LE_bis = TestFile("UTF16_LE_bis.txt", BOMEncoding.UTF_16_LE, 1)
  // Files with BOM manually cleaned
  val UTF8_with_BOM_manually_cleaned = TestFile("UTF8_with_BOM_manually_cleaned.txt", BOMEncoding.ASCII, 1)
  val UTF16_BE_manually_cleaned = TestFile("UTF16_BE_manually_cleaned.txt", BOMEncoding.ASCII, 1)
  val UTF16_LE_manually_cleaned = TestFile("UTF16_LE_manually_cleaned.txt", BOMEncoding.ASCII, 1)
}

/**
 * Third list of text files with or without BOM and bad parameters.
 */
object ThirdListFilesToTestWrongParameters {
  val UTF8_with_BOM_error = TestFile("UTF8_with_BOM.txt", BOMEncoding.UTF_16_BE, 2)
  val UTF8_without_BOM_error = TestFile("UTF8_without_BOM.txt", BOMEncoding.UTF32LEUnusual, 2)
  val UTF16_BE_error = TestFile("UTF16_BE.txt", BOMEncoding.UTF_16_LE, 2)
  val UTF16_LE_error = TestFile("UTF16_LE.txt", BOMEncoding.UTF8, 2)
  val ASCII_error = TestFile("ASCII.txt", BOMEncoding.UTF8NoBOM, 2)
  val Windows_1252_error = TestFile("Windows_1252.txt", BOMEncoding.UTF_16_BE, 2)
}