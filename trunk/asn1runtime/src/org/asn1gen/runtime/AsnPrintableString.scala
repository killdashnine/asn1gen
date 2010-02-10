package org.asn1gen.runtime

class AsnPrintableString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnPrintableString = meta.AsnPrintableString

  def copy(value: String = this.value) = new AsnPrintableString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnPrintableString => this.value == that.value
    case _ => false
  }
}

object AsnPrintableString extends AsnPrintableString("") {
  def apply(value: String): AsnPrintableString = new AsnPrintableString(value)
}
