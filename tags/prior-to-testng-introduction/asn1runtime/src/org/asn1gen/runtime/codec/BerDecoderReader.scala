package org.asn1gen.runtime.codec

import java.io._
import org.asn1gen.extra._

case class BerDecoderReader(is: InputStream) extends Extras {
  def readTripletWindow(): OctetWindow = {
    val streamBuffer = InputStreamBuffer(is, new Array[Byte](64))
    
    // Read tag bytes
    var firstTagByte = streamBuffer.read()
    if ((firstTagByte & 0x1f) > 30) {
      while (streamBuffer.read() definesBit 7) {
      }
    }
    
    // Read length bytes
    val lengthByte = streamBuffer.read()
    val length = (
      if (lengthByte definesBit 7) {
        val lengthSize = lengthByte & 0x7f
        if (lengthSize == 0) {
          throw new Exception("Indefinite length currently not supported")
        }
        
        var partialLength = 0
        (0 until lengthSize) foreach { i =>
          partialLength = partialLength << 8
          partialLength += streamBuffer.read()
        }
        
        partialLength
      } else {
        lengthByte
      }
    )
    
    // Read the value bytes
    streamBuffer.reserveRemainingCapacity(length)
    val readLength = streamBuffer.read(length)
    if (length != readLength) {
      throw new IndexOutOfBoundsException(
          "(length = " + length + ") == (readLength == " + readLength + ")")
    }
    OctetWindow(streamBuffer.buffer, 0, streamBuffer.index)
  }
  
  def readOctetWindow(length: Int): OctetWindow = {
    val buffer = new Array[Byte](length)
    is.read(buffer)
    OctetWindow(buffer, 0, length)
  }
  
  private def readTagTail(buffer: Array[Byte], head: Long): Long = {
    is.read(buffer, 0, 1)
    val octet = buffer(0)
    val value = (head << 7) | (octet & 0x7f)
    if ((octet & 0x80) == 0) {
      readTagTail(buffer, value)
    } else {
      value
    }
  }
  
  def readTag(buffer: Array[Byte]): Tag = {
    assert(buffer.length > 0)
    is.read(buffer, 0, 1)
    var value: Long = buffer(0)
    val tagClass = ((value >> 6) & 0x3) match {
      case 0 => TagClass.Universal
      case 1 => TagClass.Application
      case 2 => TagClass.ContextSpecific
      case 3 => TagClass.Private
    }
    val primitive = ((value >> 5) & 0x1) == 1
    value = (value >> 5) & 0x1f
    if (0 <= value && value <= 30) {
      return Tag(tagClass, primitive, value)
    } else {
      return Tag(tagClass, primitive, readTagTail(buffer, value))
    }
    
  }
  
  def readLength(buffer: Array[Byte]): Long = {
    assert(buffer.length > 0)
    is.read(buffer, 0, 1)
    val length : Long= buffer(0)
    if ((length & 0x80) != 0) {
      return (length << 7) | readLength(buffer)
    } else {
      return length
    }
  }
}
