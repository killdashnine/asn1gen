package org.asn1gen.runtime.codec

import java.io.InputStream
import java.io.EOFException

class DecodingInputStream(is: InputStream, private var _index: Int = 0) {
  def index = _index
  
  def read(): Int = {
    val result = is.read
    if (result == -1) {
      throw new EOFException
    }
    return result
  }
  
  def readByte(): Byte = {
    read.toByte
  }
}
