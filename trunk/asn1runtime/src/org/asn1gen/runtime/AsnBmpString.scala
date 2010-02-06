package org.asn1gen.runtime

case class AsnBmpString() extends AsnCharacterString {
  override def _desc: meta.AsnBmpString = meta.AsnBmpString
}

object AsnBmpString extends AsnBmpString() {
}
