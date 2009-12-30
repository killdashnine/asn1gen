package org.asn1gen.runtime.codec

import scala.util.parsing.combinator._
import org.asn1gen.parsing.ParsersUtil

trait PackratBerDecoder extends PackratParsers with ParsersUtil {
  type Elem = Byte
  
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
      println("result = " + result)
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
  def _null(length: Int): Parser[Unit] =
    ( require(length == 0, "Null value encoding must be zero length")
    ~>success(())
    )
  
  // Boolean
  def boolean(length: Int): Parser[Boolean] = {
    ( require(length == 1, "Boolean encoding must have length of 1 byte")
    ~>anyElem ^^ { v => v != 0 }
    )
  }
}
