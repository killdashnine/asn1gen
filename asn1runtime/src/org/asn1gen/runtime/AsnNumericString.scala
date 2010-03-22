package org.asn1gen.runtime

class AsnNumericString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnNumericString = meta.AsnNumericString

  def copy(value: String = this.value) = new AsnNumericString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnNumericString => this.value == that.value
    case _ => false
  }
}

object AsnNumericString extends AsnNumericString("") {
  def apply(value: String): AsnNumericString = new AsnNumericString(value)
}
