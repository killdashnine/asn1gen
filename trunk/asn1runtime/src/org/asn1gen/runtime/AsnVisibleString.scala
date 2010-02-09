package org.asn1gen.runtime

class AsnVisibleString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnVisibleString = meta.AsnVisibleString

  def copy(value: String = this.value) = new AsnVisibleString(value)

  def equals(that: AsnVisibleString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnVisibleString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnVisibleString {
  def apply(value: String): AsnVisibleString = new AsnVisibleString(value)
}
