package org.asn1gen.runtime.codec

import org.asn1gen.runtime._
import org.asn1gen.extra._
import java.io._
import scala.annotation.tailrec

trait BerDecoderBase {
  import org.asn1gen.extra.Extras._
  
  def decode(is: DecodingInputStream, template: AsnBoolean): AsnBoolean = {
    val triplet = decodeTriplet(is)
    assert(triplet.describes(template))
    is.span(triplet.length) {
      val value = is.readByte
      AsnBoolean(value != 0)
    }
  }
  
  def decode(is: DecodingInputStream, template: AsnNull): AsnNull = {
    val triplet = decodeTriplet(is)
    assert(triplet.describes(template))
    is.span(triplet.length) {
      AsnNull
    }
  }
  
  def decode(is: DecodingInputStream, template: AsnInteger): AsnInteger = {
    val triplet = decodeTriplet(is)
    assert(triplet.describes(template))
    is.span(triplet.length) {
      val buffer = new Array[Byte](triplet.length)
      is.read(buffer)
      var value: Long = if (buffer(0) > 0) 0 else -1
      buffer foreach { byte =>
        value = (value << 8) | byte
      }
      AsnInteger(value)
    }
  }
  
  final def decodeSequence[T <: AsnSequence](is: DecodingInputStream, template: T)(f: Int => T): T = {
    val triplet = decodeTriplet(is)
    assert(triplet.describes(template))
    f(0)
  }
  
  def decodeSequenceField[T](is: DecodingInputStream, tag: Int)(f: Int => T): T = {
    val triplet = decodeTriplet(is)
    return f(0)
  }
  
  def decodeTriplet[T](is: DecodingInputStream): Triplet = {
    // Read tag bytes
    var firstTagByte = is.readByte
    val tagClass = ((firstTagByte >> 6) & 0x3) match {
      case 0 => TagClass.Universal
      case 1 => TagClass.Application
      case 2 => TagClass.ContextSpecific
      case 3 => TagClass.Private
    }
    val tagConstructed = (firstTagByte & 0x20) != 0
    var tagValue = firstTagByte & 0x1f
    
    if (tagValue == 31) {
      var tagByte = is.readByte
      while (tagByte definesBit 7) {
        tagValue = (tagValue << 7) | (tagByte & 0x7f)
        tagByte = is.readByte
      }
    }
    
    // Read length bytes
    val lengthByte = is.readByte
    val length = (
      if (lengthByte definesBit 7) {
        val lengthSize = lengthByte & 0x7f
        if (lengthSize == 0) {
          throw new Exception("Indefinite length currently not supported")
        }
        
        var partialLength = 0
        (0 until lengthSize) foreach { i =>
          partialLength = partialLength << 8
          partialLength += is.readByte
        }
        
        partialLength
      } else {
        lengthByte
      }
    )
    
    Triplet(tagClass, tagConstructed, tagValue, length)
  }
}

