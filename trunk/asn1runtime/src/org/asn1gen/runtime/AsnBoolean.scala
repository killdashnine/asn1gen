package org.asn1gen.runtime

case class AsnBoolean(value: Boolean) extends AsnType {
  override def _desc: meta.AsnBoolean = meta.AsnBoolean
}

object AsnBoolean extends AsnBoolean(false) {
}
