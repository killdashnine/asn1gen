package org.asn1gen.runtime.meta

trait AsnGeneralString extends AsnCharacterString {
  override def name: String = "AsnGeneralString"
}

object AsnGeneralString extends AsnGeneralString
