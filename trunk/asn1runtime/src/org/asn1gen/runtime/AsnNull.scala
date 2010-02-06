package org.asn1gen.runtime

sealed case class AsnNull() extends AsnType {
  override def _desc: meta.AsnNull = meta.AsnNull
}

object AsnNull extends AsnNull {
}
