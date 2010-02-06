package org.asn1gen.runtime.meta

trait AsnTeletexString extends AsnCharacterString {
  override def name: String = "AsnTeletexString"
}

object AsnTeletexString extends AsnTeletexString
