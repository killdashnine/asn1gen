package org.asn1gen.runtime

class AsnVideotexString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnVideotexString = meta.AsnVideotexString

  def copy(value: String = this.value) = new AsnVideotexString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnVideotexString => this.value == that.value
    case _ => false
  }
}

object AsnVideotexString extends AsnVideotexString("") {
  def apply(value: String): AsnVideotexString = new AsnVideotexString(value)
}
