package org.asn1gen.runtime.meta

trait AsnNumericString extends AsnCharacterString {
  override def name: String = "AsnNumericString"
}

object AsnNumericString extends AsnNumericString
