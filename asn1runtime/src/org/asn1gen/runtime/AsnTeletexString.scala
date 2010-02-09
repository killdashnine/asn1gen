package org.asn1gen.runtime

class AsnTeletexString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnTeletexString = meta.AsnTeletexString

  def copy(value: String = this.value) = new AsnTeletexString(value)

  def equals(that: AsnTeletexString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      that.asInstanceOf[AsnTeletexString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnTeletexString {
  def apply(value: String): AsnTeletexString = new AsnTeletexString(value)
}
