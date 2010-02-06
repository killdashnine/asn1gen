package org.asn1gen.runtime.meta

trait AsnVisibleString extends AsnCharacterString {
  override def name: String = "AsnVisibleString"
}

object AsnVisibleString extends AsnVisibleString
