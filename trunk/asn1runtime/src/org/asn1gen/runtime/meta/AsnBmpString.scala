package org.asn1gen.runtime.meta

trait AsnBmpString extends AsnCharacterString {
  override def name: String = "AsnBmpString"
}

object AsnBmpString extends AsnBmpString
