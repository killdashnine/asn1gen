package org.asn1gen.runtime

class AsnBmpString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnBmpString = meta.AsnBmpString

  def copy(value: String = this.value) = new AsnBmpString(value)

  def equals(that: AsnBmpString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnBmpString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnBmpString extends AsnBmpString("") {
  def apply(value: String): AsnBmpString = new AsnBmpString(value)
}
