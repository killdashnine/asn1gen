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
    decodeTriplet(is) { (tag, length) =>
      assert(tag == 1)
      assert(length == 1)
      val dis = new DecodingInputStream(is)
      val value = dis.readByte
      AsnBoolean(value != 0)
    }
  }

  def decodeTriplet[T](is: InputStream)(f: (Int, Int) => T): T = {
    val tis = new DecodingInputStream(is)
    
    // Read tag bytes
    var firstTagByte = tis.read()
    if ((firstTagByte & 0x1f) > 30) {
      while (tis.readByte definesBit 7) {
      }
    }
    
    // Read length bytes
    val lengthByte = tis.readByte
    val length = (
      if (lengthByte definesBit 7) {
        val lengthSize = lengthByte & 0x7f
        if (lengthSize == 0) {
          throw new Exception("Indefinite length currently not supported")
        }
        
        var partialLength = 0
        (0 until lengthSize) foreach { i =>
          partialLength = partialLength << 8
          partialLength += tis.readByte
        }
        
        partialLength
      } else {
        lengthByte
      }
    )
    
    f(0, length)
  }
}

