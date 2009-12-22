package org.asn1gen.runtime.codec

import java.io.InputStream
import java.io.EOFException

class DecodingInputStream(private _is: InputStream, private var _index: Int = 0) extends InputStream {
  def index: Int = _index
  
  def read(): Int = {
    val result = _is.read
    if (result == -1) {
      throw new EOFException
    }
    _index += 1
    return result
  }
  
  def readByte(): Byte = {
    read.toByte
  }
  
  def span[T](length: Int)(f: => T): T = {
    val newIndex = _index + length
    try {
      f
    } finally {
      assert(_index == newIndex)
    }
  }
}
