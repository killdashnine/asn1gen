package org.asn1gen.runtime.codec

import java.io._

case class InputStreamBuffer(
    is: InputStream,
    var buffer: Array[Byte] = new Array[Byte](32),
    var index: Int = 0) {
  def reserve(length: Int) = {
    if (length > buffer.length) {
      val newBuffer = new Array[Byte](length)
      Array.copy(buffer, 0, newBuffer, 0, index)
      buffer = newBuffer
    }
  }
  
  def reserveRemainingCapacity(length: Int) = {
    reserve(index + length)
  }
  
  def ensureCapacity(length: Int) = {
    if (length > buffer.length) {
      val newBuffer = new Array[Byte](length * 2)
      Array.copy(buffer, 0, newBuffer, 0, index)
      buffer = newBuffer
    }
  }
  
  def ensureRemainingCapacity(length: Int) = {
    ensureCapacity(index + length)
  }
  
  def read(): Byte = {
    ensureRemainingCapacity(1)
    val result = is.read()
    if (result == -1) {
      throw new EOFException()
    } else {
      val value = result.toByte
      buffer(index) = value
      index += 1
      return value
    }
  }
  
  def read(length: Int): Int = {
    ensureRemainingCapacity(index + length)
    is.read(buffer, index, length)
    val oldIndex = index
    index += length
    return oldIndex
  }
}
