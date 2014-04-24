package com.taj.unicode_detector.CommandLine

import org.slf4j.impl.SimpleLogger
import java.io.File
import com.taj.unicode_detector.Encoding.Operations
import com.taj.unicode_detector.Converter
import com.typesafe.scalalogging.slf4j.LazyLogging

/**
 * Execute the command send through the command line.
 */
object CommandLineExecutor extends LazyLogging {
  def apply(args: Array[String]) {
    val opts = CommandLineParser(args)

    System.setProperty(SimpleLogger.DEFAULT_LOG_LEVEL_KEY, if (opts.debug.get.getOrElse(false)) "debug" else "info")

    // delete existing output file
    val optionOutput = opts.output.get
    optionOutput.map(new File(_)).filter(_.exists()).foreach(_.delete())

    opts
      .encoding
      .get
      .map(listOfFiles ⇒ listOfFiles.foreach(f ⇒ Operations.miniDetect(f, optionOutput)))

    val convert8859_15 = opts.convert8859_15.get
    convert8859_15 match {
      case Some(list: List[String]) ⇒
        list.foreach(file ⇒ Converter.Converter.convert2ISO_8859_15(file,
          new File(opts.output.get.get, new File(file).getName).getAbsolutePath))
      case None ⇒
    }

    val convertUTF8 = opts.convertUTF8.get
    convertUTF8 match {
      case Some(list: List[String]) ⇒
        list.foreach(file ⇒ Converter.Converter.convert2UTF_8(file,
          new File(opts.output.get.get, new File(file).getName).getAbsolutePath))
      case None ⇒
    }

    val optionMerge = opts.merge.get
    optionMerge match {
      case Some(list) ⇒
        if (!Operations.isSameEncoding(true, list: _*)) System.exit(1)
        Operations.mergeFilesWithoutBom(opts.output.get.get, list: _*)
      case None ⇒
    }
  }
}
