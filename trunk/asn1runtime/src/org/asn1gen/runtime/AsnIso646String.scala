package org.asn1gen.runtime

class AsnIso646String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnIso646String = meta.AsnIso646String

  def copy(value: String = this.value) = new AsnIso646String(value)

  def equals(that: AsnIso646String) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnIso646String].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnIso646String {
  def apply(value: String): AsnIso646String = new AsnIso646String(value)
}
