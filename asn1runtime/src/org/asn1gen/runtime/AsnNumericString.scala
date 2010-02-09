package org.asn1gen.runtime

class AsnNumericString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnNumericString = meta.AsnNumericString

  def copy(value: String = this.value) = new AsnNumericString(value)

  def equals(that: AsnNumericString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnNumericString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnNumericString {
  def apply(value: String): AsnNumericString = new AsnNumericString(value)
}
