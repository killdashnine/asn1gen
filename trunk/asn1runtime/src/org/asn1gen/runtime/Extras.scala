package org.asn1gen.runtime

object Extras {
  implicit def toAsn(value: Boolean): AsnBoolean = if (value) AsnTrue else AsnFalse
}
