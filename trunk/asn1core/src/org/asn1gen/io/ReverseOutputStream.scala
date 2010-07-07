package org.asn1gen.io

import java.io.Closeable
import java.io.Flushable

trait ReverseOutputStream extends Closeable with Flushable {
  def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
    val end = offset + length
    val invalidOffset = (offset < 0) || (offset > bytes.length)
    val invalidLength = length < 0
    val invalidEnd = (end > bytes.length) || (end < 0)
    if (invalidOffset || invalidLength || invalidEnd) {
      throw new IndexOutOfBoundsException()
    } else if (length == 0) {
      return
    }
    for (i <- (length - 1) to 0) {
      write(bytes(offset + i))
    }
  }

  def write(bytes: Array[Byte], offset: Int): Unit =
    write(bytes, offset, bytes.length - offset)

  def write(bytes: Array[Byte]): Unit =
    write(bytes, 0, bytes.length)

  def write(byte: Int): Unit
}
