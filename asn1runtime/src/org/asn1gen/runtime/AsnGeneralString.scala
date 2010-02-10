package org.asn1gen.runtime

class AsnGeneralString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnGeneralString = meta.AsnGeneralString

  def copy(value: String = this.value) = new AsnGeneralString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnGeneralString => this.value == that.value
    case _ => false
  }
}

object AsnGeneralString extends AsnGeneralString("") {
  def apply(value: String): AsnGeneralString = new AsnGeneralString(value)
}
