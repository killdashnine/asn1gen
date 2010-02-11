package org.asn1gen.runtime

class AsnOctetString(val value: List[Byte]) extends AsnType {
  override def _desc: meta.AsnOctetString = meta.AsnOctetString

  def copy(value: List[Byte] = this.value) = new AsnOctetString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnOctetString => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = this.value.hashCode

  def value(f: (List[Byte] => List[Byte])): AsnOctetString = this.copy(value = f(this.value))

  def unapply(): Option[(List[Byte])] = Some(value)
  
  def string: String = new String(this.value.toArray)
}

object AsnOctetString extends AsnOctetString(Nil) {
  def apply(value: List[Byte]): AsnOctetString = new AsnOctetString(value)
  
  def apply(value: String): AsnOctetString = AsnOctetString(value.getBytes.toList)
}
