package org.asn1gen.runtime

sealed case class AsnNull() extends AsnType {
  override def _desc: meta.AsnNull = meta.AsnNull
  
  def _raw: this.type = this
}

object AsnNull extends AsnNull {
}
