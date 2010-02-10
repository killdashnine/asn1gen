package org.asn1gen.runtime

class AsnT61String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnT61String = meta.AsnT61String

  def copy(value: String = this.value) = new AsnT61String(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnT61String => this.value == that.value
    case _ => false
  }
}

object AsnT61String {
  def apply(value: String): AsnT61String = new AsnT61String(value)
}
