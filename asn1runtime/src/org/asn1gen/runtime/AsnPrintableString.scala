package org.asn1gen.runtime

case class AsnPrintableString(value: String) extends AsnCharacterString {
  override def _desc: meta.AsnPrintableString = meta.AsnPrintableString
}

object AsnPrintableString extends AsnPrintableString("") {
}
