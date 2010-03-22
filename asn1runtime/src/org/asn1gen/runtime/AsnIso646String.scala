package org.asn1gen.runtime

class AsnIso646String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnIso646String = meta.AsnIso646String

  def copy(value: String = this.value) = new AsnIso646String(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnIso646String => this.value == that.value
    case _ => false
  }
}

object AsnIso646String extends AsnIso646String("") {
  def apply(value: String): AsnIso646String = new AsnIso646String(value)
}
