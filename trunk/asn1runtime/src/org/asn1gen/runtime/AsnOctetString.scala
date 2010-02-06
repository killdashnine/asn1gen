package org.asn1gen.runtime

case class AsnOctetString(value: List[Byte]) extends AsnCharacterString {
  override def _desc: meta.AsnOctetString = meta.AsnOctetString
}

object AsnOctetString extends AsnOctetString(Nil) {
}
