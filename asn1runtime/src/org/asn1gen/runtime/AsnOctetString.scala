package org.asn1gen.runtime

case class AsnOctetString(value: List[Byte]) extends AsnCharacterString {
}

object AsnOctetString extends AsnOctetString(Nil) {
}
