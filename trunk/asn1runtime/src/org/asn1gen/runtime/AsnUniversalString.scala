package org.asn1gen.runtime

class AsnUniversalString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnUniversalString = meta.AsnUniversalString

  def copy(value: String = this.value) = new AsnUniversalString(value)

  def equals(that: AsnUniversalString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnUniversalString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnUniversalString {
  def apply(value: String): AsnUniversalString = new AsnUniversalString(value)
}
