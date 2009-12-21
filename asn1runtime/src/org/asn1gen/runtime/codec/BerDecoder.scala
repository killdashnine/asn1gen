package org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.extra._
import java.io._

class BerDecoder {
  import org.asn1gen.extra.Extras._
  
  def decode(is: InputStream, template: AsnBoolean): AsnBoolean = {
    decodeTriplet(is) { triplet =>
      assert(triplet.primitive)
      assert(triplet.tagType == 1)
      assert(triplet.length == 1)
      val dis = new DecodingInputStream(is)
      val value = dis.readByte
      AsnBoolean(value != 0)
    }
  }
  
  def decode(is: InputStream, template: AsnNull): AsnNull = {
    decodeTriplet(is) { triplet =>
      assert(triplet.primitive)
      assert(triplet.tagType == 5)
      assert(triplet.length == 0)
      AsnNull
    }
  }
  
  def decode(is: InputStream, template: AsnInteger): AsnInteger = {
    decodeTriplet(is) { triplet =>
      assert(triplet.primitive)
      assert(triplet.tagType == 2)
      assert(triplet.length > 0)
      val buffer = new Array[Byte](triplet.length)
      is.read(buffer)
      var value: Long = if (buffer(0) > 0) 0 else -1
      buffer foreach { byte =>
        value = (value << 8) | byte
      }
      AsnInteger(value)
    }
  }
  
  def decodeTriplet[T](is: InputStream)(f: Triplet => T): T = {
    val dis = new DecodingInputStream(is)
    
    // Read tag bytes
    var firstTagByte = dis.readByte
    val tagClass = ((firstTagByte >> 6) & 0x3) match {
      case 0 => TagClass.Universal
      case 1 => TagClass.Application
      case 2 => TagClass.ContextSpecific
      case 3 => TagClass.Private
    }
    val tagConstructed = (firstTagByte & 0x20) != 0
    var tagValue = firstTagByte & 0x1f
    
    if ((firstTagByte & 0x1f) > 30) {
      var tagByte = dis.readByte
      while (tagByte definesBit 7) {
        tagValue = (tagValue << 7) | (tagByte & 0x7f)
        tagByte = dis.readByte
      }
    }
    
    // Read length bytes
    val lengthByte = dis.readByte
    val length = (
      if (lengthByte definesBit 7) {
        val lengthSize = lengthByte & 0x7f
        if (lengthSize == 0) {
          throw new Exception("Indefinite length currently not supported")
        }
        
        var partialLength = 0
        (0 until lengthSize) foreach { i =>
          partialLength = partialLength << 8
          partialLength += dis.readByte
        }
        
        partialLength
      } else {
        lengthByte
      }
    )
    
    f(Triplet(tagClass, tagConstructed, tagValue, length))
  }
}

