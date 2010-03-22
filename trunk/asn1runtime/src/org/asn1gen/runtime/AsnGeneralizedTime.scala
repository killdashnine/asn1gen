package org.asn1gen.runtime

// TODO: Interim definition
class AsnGeneralizedTime(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnGeneralizedTime = meta.AsnGeneralizedTime

  def copy(value: String = this.value) = new AsnGeneralizedTime(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnGeneralizedTime => this.value == that.value
    case _ => false
  }
}

object AsnGeneralizedTime extends AsnGeneralizedTime("") {
  def apply(value: String): AsnGeneralizedTime = new AsnGeneralizedTime(value)
}
