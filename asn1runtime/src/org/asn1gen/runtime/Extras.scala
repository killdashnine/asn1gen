package org.asn1gen.runtime

import scala.collection.immutable.BitSet

object Extras {
  implicit def toAsnBitString(value: BitSet): AsnBitString =
    AsnBitString(value)

  implicit def toAsnBmpString(value: String): AsnBmpString =
    AsnBmpString(value)

  implicit def toAsnBoolean(value: Boolean): AsnBoolean =
    AsnBoolean(value)
  
  implicit def toAsnGeneralizedTime(value: String): AsnGeneralizedTime =
    AsnGeneralizedTime(value)

  implicit def toAsnGeneralString(value: String): AsnGeneralString =
    AsnGeneralString(value)

  implicit def toAsnGraphicString(value: String): AsnGraphicString =
    AsnGraphicString(value)

  implicit def toAsnInteger(value: Int): AsnInteger =
    AsnInteger(value)

  implicit def toAsnInteger(value: Long): AsnInteger =
    AsnInteger(value)
  
  implicit def toAsnIa5String(value: String): AsnIa5String =
    AsnIa5String(value)

  implicit def toAsnIso646String(value: String): AsnIso646String =
    AsnIso646String(value)

  implicit def toAsnNumericString(value: String): AsnNumericString =
    AsnNumericString(value)

  implicit def toAsnOctetString(value: String): AsnOctetString =
    AsnOctetString(value)

  implicit def toAsnOctetString(value: List[Byte]): AsnOctetString =
    AsnOctetString(value)

  implicit def toAsnPrintableString(value: String): AsnPrintableString =
    AsnPrintableString(value)

  implicit def toAsnReal(value: Double): AsnReal =
    AsnReal(value)

  implicit def toAsnT61String(value: String): AsnT61String =
    AsnT61String(value)

  implicit def toAsnTeletexString(value: String): AsnTeletexString =
    AsnTeletexString(value)

  implicit def toAsnUniversalString(value: String): AsnUniversalString =
    AsnUniversalString(value)

  implicit def toAsnUtcTime(value: String): AsnUtcTime =
    AsnUtcTime(value)

  implicit def toAsnUtf8String(value: String): AsnUtf8String =
    AsnUtf8String(value)

  implicit def toAsnVideotexString(value: String): AsnVideotexString =
    AsnVideotexString(value)

  implicit def toAsnVisibleString(value: String): AsnVisibleString =
    AsnVisibleString(value)
}
