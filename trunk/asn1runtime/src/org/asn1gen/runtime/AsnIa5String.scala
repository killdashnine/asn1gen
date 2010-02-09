package org.asn1gen.runtime

class AsnIa5String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnIa5String = meta.AsnIa5String

  def copy(value: String = this.value) = new AsnIa5String(value)

  def equals(that: AsnIa5String) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnIa5String].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnIa5String {
  def apply(value: String): AsnIa5String = new AsnIa5String(value)
}
