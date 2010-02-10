package org.asn1gen.runtime

class AsnUniversalString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnUniversalString = meta.AsnUniversalString

  def copy(value: String = this.value) = new AsnUniversalString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnUniversalString => this.value == that.value
    case _ => false
  }
}

object AsnUniversalString {
  def apply(value: String): AsnUniversalString = new AsnUniversalString(value)
}
