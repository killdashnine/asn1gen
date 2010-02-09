package org.asn1gen.runtime

class AsnVideotexString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnVideotexString = meta.AsnVideotexString

  def copy(value: String = this.value) = new AsnVideotexString(value)

  def equals(that: AsnVideotexString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnVideotexString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnVideotexString {
  def apply(value: String): AsnVideotexString = new AsnVideotexString(value)
}
