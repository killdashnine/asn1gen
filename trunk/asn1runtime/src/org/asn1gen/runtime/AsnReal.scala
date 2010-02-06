package org.asn1gen.runtime

case class AsnReal(value: Double) extends AsnType {
  override def _desc: meta.AsnReal = meta.AsnReal
}

object AsnReal extends AsnReal(0.0) {
}
