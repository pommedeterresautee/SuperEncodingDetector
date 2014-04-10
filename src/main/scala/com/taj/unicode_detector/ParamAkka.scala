package com.taj.unicode_detector

object ParamAkka {

  type EncodingTest = Array[Byte] ⇒ Int

  val bufferSize: Int = 1024

  /**
   * Size of each part sent to each Actor.
   * Speed test for different parameters based on a 400 Mb file (time in ms).
   * Size   Time
   * 2   Kb 151 021
   * 1   Mb  25 205
   * 10  Mb  22 207
   * 20  Mb  21 384 <- Best
   * 70  Mb  22 691
   * 100 Mb  23 671
   */
  val sizeOfaPartToAnalyze = 1024 * 1024 * 20

  /**
   * Compute the number of AKKA workers needed to process the file.
   * Computation based on the size of the file and the size of the segments to analyze.
   * If we are working on a small file, start less workers, if it s a big file, use the number of processor cores.
   * @param fileSize size of the file to process
   * @return the number of workers.
   */
  def numberOfWorkerRequired(fileSize: Long) =
    (1 to Runtime.getRuntime.availableProcessors)
      .find(_ * sizeOfaPartToAnalyze >= fileSize)
      .getOrElse(Runtime.getRuntime.availableProcessors)

  val checkASCII: EncodingTest = _.indexWhere(_.toInt < 0)

  val checkUTF8: EncodingTest = {
    byteArray ⇒
      val realArraySize = byteArray.takeWhile(_ != 0).size
      var passBytesAlreadyMatched = 4 // the first 4 bytes of the block are passed in case they are related to a truncated unicode char from another block
      var passBytesMayMatch = -1 // if not yet the entire sequence, wait to advance 4 bytes to say if there is definitely no match
      (0 to (realArraySize - 4))
        .map(i ⇒ (byteArray(i), byteArray(i + 1), byteArray(i + 2), byteArray(i + 3)))
        .indexWhere {
          case (b1, b2, b3, b4) if b1.toInt >= 0 & b2.toInt >= 0 & b3.toInt >= 0 & b4.toInt >= 0 ⇒ // ASCII
            passBytesMayMatch = -1
            passBytesAlreadyMatched = 0
            false
          case (b1, b2, b3, b4) if (b1 & 0xFF) >= 192 & (b2 & 0xFF) >= 128 ⇒
            passBytesMayMatch = -1
            passBytesAlreadyMatched = 1
            false
          case (b1, b2, b3, b4) if (b1 & 0xFF) >= 224 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 ⇒
            passBytesMayMatch = -1
            passBytesAlreadyMatched = 2
            false
          case (b1, b2, b3, b4) if (b1 & 0xFF) >= 240 & (b2 & 0xFF) >= 128 & (b3 & 0xFF) >= 128 & (b4 & 0xFF) >= 128 ⇒
            passBytesMayMatch = -1
            passBytesAlreadyMatched = 3
            false
          case (b1, b2, b3, b4) ⇒
            if (passBytesAlreadyMatched > 0) {
              passBytesAlreadyMatched -= 1
              false
            }
            else if (passBytesMayMatch == -1) {
              passBytesMayMatch = 3 // pass the check for the next 3 bytes to get a sequence of 4 bytes.
              false
            }
            else if (passBytesMayMatch > 0) {
              passBytesMayMatch -= 1 // decrease the count waiting for the entire sequence
              false
            }
            else {
              true
            }
        }
  }
}