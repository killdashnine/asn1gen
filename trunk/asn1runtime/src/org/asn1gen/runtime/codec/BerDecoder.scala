package org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.extra._
import java.io._

class BerDecoder {
  import org.asn1gen.extra.Extras._
  
  def tripletFor(window: OctetWindow): Triplet = {
    Triplet(window)
  }
  
  def decodeAs(triplet: Triplet, value: AsnInteger): AsnInteger = {
    assert(triplet.tagValue == 2)
    AsnInteger(1)
  }
  
  def decode(window: OctetWindow): AsnType = {
    null
  }
  
  def decode(is: InputStream): AsnType = {
    decode(BerDecoderReader(is).readTripletWindow())
  }




  def decode(is: InputStream, template: AsnBoolean): AsnBoolean = {
    decodeTriplet(is) { (tagClass, constructed, tagValue, length) =>
      assert(!constructed)
      assert(tagValue == 1)
      assert(length == 1)
      val dis = new DecodingInputStream(is)
      val value = dis.readByte
      AsnBoolean(value != 0)
    }
  }

  def decodeTriplet[T](is: InputStream)(f: (TagClass, Boolean, Int, Int) => T): T = {
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
      while (dis.readByte definesBit 7) {
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
    
    f(tagClass, tagConstructed, tagValue, length)
  }
}

