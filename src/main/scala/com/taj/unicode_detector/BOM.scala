package com.taj.unicode_detector

import java.io.{File, FileOutputStream, FileInputStream}
import java.nio.file.{Paths, Path, Files}

/**
 * Contain property of each BOM.
 * @param name Name of the encoding type.
 * @param BOM List of values of the first bytes when file is not an XML.
 * @param BOM_XML List of values of the first bytes when file is an XML.
 */
case class BOMFileEncoding(name: String, BOM: List[Int], BOM_XML: List[Int])

/**
 * Main class to detect a file encoding based on its BOM.
 */
object BOM {
  val bigUCS4 = BOMFileEncoding("UCS-4", List(0x00, 0x00, 0xFE, 0xFF), List(0x00, 0x00, 0x00, '<'))
  val littleUCS4 = BOMFileEncoding("UCS-4", List(0xFF, 0xFE, 0x00, 0x00), List('<', 0x00, 0x00, 0x00))
  val unusualUCS4 = BOMFileEncoding("UCS-4", List(0x00, 0x00, 0xFF, 0xFE), List(0x00, 0x00, '<', 0x00))
  val unusualUCS4_bis = BOMFileEncoding("UCS-4", List(0xFE, 0xFF, 0x00, 0x00), List(0x00, '<', 0x00, 0x00))
  val UTF16BE = BOMFileEncoding("UTF-16BE", List(0xFE, 0xFF), List(0x00, '<', 0x00, '?'))
  val UTF16LE = BOMFileEncoding("UTF-16LE", List(0xFF, 0xFE), List('<', 0x00, '?', 0x00))
  val UTF8 = BOMFileEncoding("UTF-8", List(0xEF, 0xBB, 0xBF), List(0x4C, 0x6F, 0xA7, 0x94))
  val ASCII = BOMFileEncoding("ASCII", List(), List('<', '?', 'x', 'm'))

  /**
   * Detects the encoding of a file based on its BOM.
   * @param file path to the file.
   * @return the encoding. If no BOM detected, send back ASCII encoding.
   */
  def detect(file: String): BOMFileEncoding = {
    val in = new FileInputStream(file)
    var ret: BOMFileEncoding = null
    val bytesToRead = 1024 // enough to read most XML encoding declarations

    // This may fail if there are a lot of space characters before the end
    // of the encoding declaration
    in mark bytesToRead
    val bytes = List(in.read, in.read, in.read, in.read)
    in.close() // To make the file deletable after processing!

    ret = bytes match {
      case bigUCS4.BOM            | bigUCS4.BOM_XML           => bigUCS4
      case littleUCS4.BOM         | littleUCS4.BOM_XML        => littleUCS4
      case unusualUCS4.BOM        | unusualUCS4.BOM_XML       => unusualUCS4
      case unusualUCS4_bis.BOM    | unusualUCS4_bis.BOM_XML   => unusualUCS4_bis
      case UTF16BE.BOM :+ _ :+ _  | UTF16BE.BOM_XML           => UTF16BE
      case UTF16LE.BOM :+ _ :+ _  | UTF16LE.BOM_XML           => UTF16LE
      case UTF8.BOM :+ _          | UTF8.BOM_XML              => UTF8
      case _                                                  => ASCII
    }
    ret
  }

  /**
   * Compare files given in parameter to the BOM in parameters to determine if they are all the same.
   * @param verbose print some helpful messages.
   * @param bom the supposed BOM
   * @param paths paths to the files to compare.
   * @return true if the encoding is the same everywhere.
   */
  def isSameBOM(verbose:Boolean, bom:BOMFileEncoding, paths:String*):Boolean = {
    if (paths.size < 2) throw new IllegalArgumentException(s"Not enough files to compare (${paths.size})")
    paths.forall{path:String =>
      val detectedBOM = detect(path)
      val same = bom.equals(detectedBOM)
      if(!same && verbose) println(s"The first file [${paths(0)}] is encoded as ${bom.name} but the file [$path] is encoded as ${detectedBOM.name}.")
      same
    }
  }

  private def removeBOM(verbose:Boolean, bom:BOMFileEncoding, path:String):FileInputStream = {
    val toDrop = bom.BOM.size
    val f = new FileInputStream(path)
    val realSkipped = f.skip(toDrop)
    if(toDrop != realSkipped) throw new IllegalStateException(s"Failed to skip the correct number of bytes ($realSkipped instead of $toDrop)")
    f
  }

  def copyWithoutBom(from:String, to:String, verbose:Boolean) {
    val bomFrom = detect(from)
    val fileTo = new File(to)
    if(fileTo.exists()) fileTo.delete()
    Thread.sleep(100l)
    if(fileTo.exists()) throw new IllegalStateException(s"File $to can't be deleted.")
    val output = new FileOutputStream(fileTo)

    val input = removeBOM(verbose, bomFrom, from)
    val bytes = new Array[Byte](1024) //1024 bytes - Buffer size
    try{
      Iterator
        .continually (input.read(bytes))
        .takeWhile (-1 !=)
        .foreach (read => output.write(bytes,0,read))
    } finally {
      input.close()
      output.close()
    }
  }

  def mergeFilesWithoutBom(verbose:Boolean, destination:String, paths:String*) {
    Files.copy(Paths.get(paths(0)), Paths.get(destination))
    val bytes = new Array[Byte](1024)
    val output = new FileOutputStream(destination, true)
    paths.drop(1).foreach{path =>
      val input = removeBOM(verbose, detect(path), path)
      Iterator
        .continually (input.read(bytes))
        .takeWhile (-1 !=)
        .foreach (read => output.write(bytes,0,read))
      input.close()
    }
    output.close()
  }
}