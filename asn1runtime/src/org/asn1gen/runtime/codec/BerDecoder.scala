package org.asn1gen.runtime.codec

import java.io._

class BerDecoder(is: InputStream) {
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
}
