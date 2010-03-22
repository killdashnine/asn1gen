package org.asn1gen.runtime

class AsnTeletexString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnTeletexString = meta.AsnTeletexString

  def copy(value: String = this.value) = new AsnTeletexString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnTeletexString => this.value == that.value
    case _ => false
  }
}

object AsnTeletexString extends AsnTeletexString("") {
  def apply(value: String): AsnTeletexString = new AsnTeletexString(value)
}
