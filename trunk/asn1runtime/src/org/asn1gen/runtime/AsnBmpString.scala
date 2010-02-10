package org.asn1gen.runtime

class AsnBmpString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnBmpString = meta.AsnBmpString

  def copy(value: String = this.value) = new AsnBmpString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnBmpString => this.value == that.value
    case _ => false
  }
}

object AsnBmpString extends AsnBmpString("") {
  def apply(value: String): AsnBmpString = new AsnBmpString(value)
}
