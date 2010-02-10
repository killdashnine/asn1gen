package org.asn1gen.runtime

class AsnInteger(val value: Long) extends AsnType {
  override def _desc: meta.AsnInteger = meta.AsnInteger

  def copy(value: Long = this.value) = AsnInteger(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnInteger => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = this.value.hashCode

  def value(f: (Long => Long)): AsnInteger = this.copy(value = f(this.value))
}

object AsnInteger extends AsnInteger(0) {
  def apply(value: Long): AsnInteger = new AsnInteger(value)
  
  def unapply(): Option[(Long)] = Some(value)
}
