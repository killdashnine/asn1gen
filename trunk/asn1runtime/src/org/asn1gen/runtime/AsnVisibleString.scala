package org.asn1gen.runtime

class AsnVisibleString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnVisibleString = meta.AsnVisibleString

  def copy(value: String = this.value) = new AsnVisibleString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnVisibleString => this.value == that.value
    case _ => false
  }
}

object AsnVisibleString extends AsnVisibleString("") {
  def apply(value: String): AsnVisibleString = new AsnVisibleString(value)
}
