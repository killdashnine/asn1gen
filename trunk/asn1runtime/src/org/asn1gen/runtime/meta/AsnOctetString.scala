package org.asn1gen.runtime.meta

trait AsnOctetString extends AsnCharacterString {
  override def name: String = "AsnOctetString"
}

object AsnOctetString extends AsnOctetString
