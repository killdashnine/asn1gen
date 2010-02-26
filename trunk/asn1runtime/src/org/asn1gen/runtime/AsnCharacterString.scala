package org.asn1gen.runtime

abstract class AsnCharacterString(val value: String) extends AsnType {
  override def _desc: meta.AsnCharacterString = meta.AsnCharacterString

  def copy(value: String = this.value): AsnCharacterString

  override def equals(that: Any): Boolean

  override def hashCode(): Int = this.value.hashCode

  def value(f: (String => String)): AsnCharacterString = this.copy(value = f(this.value))

  def unapply(): Option[(String)] = Some(value)
  
  def length: Int = this.value.length
}

object AsnCharacterString {
}
