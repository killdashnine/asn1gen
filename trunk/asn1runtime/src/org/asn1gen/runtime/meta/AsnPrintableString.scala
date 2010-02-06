package org.asn1gen.runtime.meta

trait AsnPrintableString extends AsnCharacterString {
  override def name: String = "AsnPrintableString"
}

object AsnPrintableString extends AsnPrintableString
