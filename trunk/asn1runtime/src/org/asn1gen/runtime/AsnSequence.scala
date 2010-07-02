package org.asn1gen.runtime

class AsnSequence extends AsnType {
  override def _desc: meta.AsnSequence = meta.AsnSequence
  
  def _raw: this.type = this
}

object AsnSequence {
}
