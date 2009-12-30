package org.asn1gen.runtime.codec

import scala.util.parsing.combinator._

trait PackratBerDecoder extends PackratParsers {
  type Elem = Byte
  
  /*def byte(value: Byte): Parser[Byte] = elem("byte " + value) {
    case `value` => value
  }
  
  def byte: Parser[Byte] = elem("byte") {
    case value: Byte => value
  }*/
  
  def tlTagLoneByte: Parser[Byte] =
    elem("Lone tag-length byte", c => (c & 31) != 31)

  def tlTagLeadingByte: Parser[Byte] =
    elem("Leading tag-length byte", c => (c & 31) == 31)

  def tlTagContinuingByte: Parser[Byte] =
    elem("Continuing tag-length byte", c => (c & 0x80) != 0)

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
        var tagValue =
          (continuing.foldLeft(firstTagByte & 0x1f)((a, b) => a | (b << 7)) << 7) | ending
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
}
