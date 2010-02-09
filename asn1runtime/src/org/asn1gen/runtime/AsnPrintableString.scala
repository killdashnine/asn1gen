package org.asn1gen.runtime

class AsnPrintableString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnPrintableString = meta.AsnPrintableString

  def copy(value: String = this.value) = new AsnPrintableString(value)

  def equals(that: AsnPrintableString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnPrintableString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnPrintableString extends AsnPrintableString("") {
  def apply(value: String): AsnPrintableString = new AsnPrintableString(value)
}
