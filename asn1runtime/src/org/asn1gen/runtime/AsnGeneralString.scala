package org.asn1gen.runtime

class AsnGeneralString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnGeneralString = meta.AsnGeneralString

  def copy(value: String = this.value) = new AsnGeneralString(value)

  def equals(that: AsnGeneralString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnGeneralString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnGeneralString extends AsnGeneralString("") {
  def apply(value: String): AsnGeneralString = new AsnGeneralString(value)
}
