package org.asn1gen.runtime.meta

trait AsnUniversalString extends AsnCharacterString {
  override def name: String = "AsnUniversalString"
}

object AsnUniversalString extends AsnUniversalString
