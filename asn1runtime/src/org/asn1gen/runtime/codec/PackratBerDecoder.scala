package org.asn1gen.runtime.codec

import scala.util.parsing.combinator._
import org.asn1gen.parsing.ParsersUtil
import org.asn1gen.parsing.BinaryParsers

trait PackratBerDecoder extends BinaryParsers with PackratParsers with ParsersUtil {
  type AsnBoolean
  type AsnInteger
  type AsnNull
  type AsnOctetString
  type AsnReal
  
  // Tag-Length Header
  
  def tlTagLoneByte: Parser[Byte] =
    elem("Lone tag-length byte", c => (c & 31) != 31)

  def tlTagLeadingByte: Parser[Byte] =
    elem("Leading tag-length byte", c => (c & 31) == 31)

  def tlTagContinuingByte: Parser[Byte] =
    ( elem("Continuing tag-length byte", c => (c & 0x80) != 0)
    ) ^^ { v => (v & 0x7f).toByte }

  def tlTagEndingByte: Parser[Byte] =
    elem("Ending tag-length byte", c => (c & 0x80) == 0)

  def tlTag: Parser[Triplet] =
    ( ( tlTagLoneByte
      ) ^^ { firstTagByte =>
        val tagClass = ((firstTagByte >> 6) & 0x3) match {
          case 0 => TagClass.Universal
          case 1 => TagClass.Application
          case 2 => TagClass.ContextSpecific
          case 3 => TagClass.Private
        }
        val tagConstructed = (firstTagByte & 0x20) != 0
        var tagValue = firstTagByte & 0x1f
        Triplet(tagClass, tagConstructed, tagValue, 0)
      }
    | ( tlTagLeadingByte ~ tlTagContinuingByte.* ~ tlTagEndingByte
      ) ^^ { case firstTagByte ~ continuing ~ ending =>
        val tagClass = ((firstTagByte >> 6) & 0x3) match {
          case 0 => TagClass.Universal
          case 1 => TagClass.Application
          case 2 => TagClass.ContextSpecific
          case 3 => TagClass.Private
        }
        val tagConstructed = (firstTagByte & 0x20) != 0
        val init = continuing.foldLeft(0)((a, b) => (a << 7) | b)
        var tagValue = (init << 7) | ending
        Triplet(tagClass, tagConstructed, tagValue, 0)
      }
    )
  
  def tlLengthContinuingByte: Parser[Byte] =
    ( elem("Leading tag-length length byte", c => (c & 0x80) != 0)
    ) ^^ { byte => (byte & 0x7f).toByte }
  
  def tlLengthEndingByte: Parser[Byte] =
    elem("Leading tag-length length byte", c => (c & 0x80) == 0)
  
  def tlLength: Parser[Int] =
    ( tlLengthContinuingByte.* ~ tlLengthEndingByte
    ) ^^ { case continuing ~ ending =>
      val init = continuing.foldLeft(0.toInt)((a, b) => (a << 7) | b)
      val result = (init << 7) | ending.toInt
      result
    }
  
  def tl =
    ( tlTag ~ tlLength
    ) ^^ { case tag ~ length => tag.copy(length = length) }
  
  // Value Length
  def length = tlLength
  
  def length[T](p: Int => Parser[T]): Parser[T] = length >> p
  
  def require(f: => Boolean, errorMessage: String): Parser[Unit] =
    if (f) {
      success(())
    } else {
      throw new DecodingException(errorMessage)
    }
  
  // Null
  def rawNull(length: Int): Parser[Unit] =
    ( require(length == 0, "Null value encoding must be zero length")
    ~>success(())
    )
  
  // Boolean
  def rawBoolean(length: Int): Parser[Boolean] =
    ( require(length == 1, "Boolean encoding must have length of 1 byte")
    ~>anyElem ^^ { v => v != 0 }
    )
  
  // Integer
  def rawInteger(length: Int): Parser[Long] =
    ( require(length > 0, "Integer encoding must have length of at least 1 byte")
    ~>repN(length, anyElem)
    ) ^^ { bytes =>
      bytes.tail.foldLeft(bytes.head.toLong) { (a, b) => (a << 8) | (b & 0xff) }
    }
  
  // Enumeration
  def rawEnumeration(length: Int): Parser[Long] = rawInteger(length)
  
  // Real
  def rawRealSpecByte: Parser[Byte] = anyElem
  
  def rawRealDataNumber(length: Int): Parser[Int] =
    ( require(length > 0, "Integer encoding must have length of at least 1 byte")
    ~>repN(length, anyElem)
    ) ^^ { bytes =>
      bytes.foldLeft(0) { (a, b) => (a << 8) | (b & 0xff) }
    }
  
  def rawRealData(length: Int)(spec: Byte): Parser[Double] = {
    if ((spec & 0xc0) == 0) {
      if (spec == 1) {
        // This needs to be properly coded to reject more possibilities.
        ( repN(length, anyElem)
        ) ^^ { bytes => java.lang.Double.parseDouble(new String(bytes.toArray)) }
      } else if (spec == 2) {
        // This needs to be properly coded to reject more possibilities.
        ( repN(length, anyElem)
        ) ^^ { bytes => java.lang.Double.parseDouble(new String(bytes.toArray)) }
      } else if (spec == 3) {
        // This needs to be properly coded to reject more possibilities.
        ( repN(length, anyElem)
        ) ^^ { bytes => java.lang.Double.parseDouble(new String(bytes.toArray)) }
      } else {
        // This needs to be properly coded to reject more possibilities.
        failure("Not a valid NR encoding")
      }
    } else if ((spec & 0x80.toByte) != 0) {
      val sign = if ((spec & 0x40.toByte) != 0) -1 else 1
      val base = ((spec >> 4) & 0x3) match {
        case 0 => 2
        case 1 => 8
        case 2 => 16
        case _ => 32 // TODO: This should be decoding error.
      }
      val scale = (spec >> 2) & 0x3
      val exponentLength = (spec & 0x3) + 1
      if (exponentLength != 4) {
        println("AAA")
        ( rawRealDataNumber(exponentLength) ~ rawRealDataNumber(length - 1)
        ) ^^ { case exponent ~ number =>
          println("sign: " + sign)
          println("scale: " + scale)
          println("base: " + base)
          println("exponent: " + exponent)
          println("number: " + number)
          sign * Math.pow(2, scale) * Math.pow(base, exponent) * number 
        }
      } else {
        println("BBB: " + spec)
        ( anyElem
        >>{ exponentLength =>
            println("exponentLength: " + exponentLength)
            ( rawRealDataNumber(exponentLength) ~ rawRealDataNumber(length - exponentLength - 1)
            ) ^^ { case exponent ~ number =>
              println("sign: " + sign)
              println("scale: " + scale)
              println("base: " + base)
              println("exponent: " + exponent)
              println("number: " + number)
              sign * Math.pow(2, scale) * Math.pow(base, exponent) * number
            }
          }
        )
      }
    } else {
      failure("Not a valid NR encoding")
    }
  }
  
  def rawReal(length: Int): Parser[Double] = {
    if (length == 0) {
      success(0)
    } else if (length == 1) {
      ( anyElem
      ) ^? {
        case value if value == 64.toByte => Double.PositiveInfinity
        case value if value == 65.toByte => Double.NegativeInfinity
      }
    } else {
      ( anyElem >> rawRealData(length - 1)
      )
    }
  }
  
  // OctetString
  def rawOctetString(length: Int): Parser[List[Byte]] = repN(length, anyElem)
  
  def asnBoolean(length: Int): Parser[AsnBoolean] = rawBoolean(length) ^^ mkAsnBoolean
  def asnInteger(length: Int): Parser[AsnInteger] = rawInteger(length) ^^ mkAsnInteger
  def asnNull(length: Int): Parser[AsnNull] = rawNull(length) ^^ mkAsnNull
  def asnOctetString(length: Int): Parser[AsnOctetString] = rawOctetString(length) ^^ mkAsnOctetString
  def asnReal(length: Int): Parser[AsnReal] = rawReal(length) ^^ mkAsnReal
  
  def mkAsnBoolean(value: Boolean): AsnBoolean
  def mkAsnInteger(value: Long): AsnInteger
  def mkAsnNull(value: Unit): AsnNull
  def mkAsnOctetString(value: List[Byte]): AsnOctetString
  def mkAsnReal(value: Double): AsnReal
}
