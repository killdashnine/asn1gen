package org.asn1gen.runtime

case class AsnPrintableString(value: String) extends AsnCharacterString {
}

object AsnPrintableString extends AsnPrintableString("") {
}
