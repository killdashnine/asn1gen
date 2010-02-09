package org.asn1gen.runtime

class AsnOctetString(val value: List[Byte]) extends AsnType {
  override def _desc: meta.AsnOctetString = meta.AsnOctetString

  def copy(value: List[Byte] = this.value) = new AsnOctetString(value)

  def equals(that: AsnOctetString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnOctetString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnOctetString extends AsnOctetString(Nil) {
  def apply(value: List[Byte]): AsnOctetString = new AsnOctetString(value)
}
