package org.asn1gen.runtime

class AsnReal(val value: Double) extends AsnType {
  override def _desc: meta.AsnReal = meta.AsnReal

  def copy(value: Double = this.value): AsnReal = AsnReal(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnReal => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = this.value.hashCode

  def value(f: (Double => Double)): AsnReal = this.copy(value = f(this.value))

  def unapply(): Option[(Double)] = Some(value)
}

object AsnReal extends AsnReal(0.0) {
  def apply(value: Double) = new AsnReal(value)
}
