package org.asn1gen.runtime

class AsnBoolean(val value: Boolean) extends AsnType {
  override def _desc: meta.AsnBoolean = meta.AsnBoolean

  def copy(value: Boolean = this.value): AsnBoolean = AsnBoolean(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnBoolean => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = this.value.hashCode

  def value(f: (Boolean => Boolean)): AsnBoolean = this.copy(value = f(this.value))
}

object AsnBoolean extends AsnBoolean(false) {
  def apply(value: Boolean) = new AsnBoolean(value)

  def unapply(value: AsnBoolean) = Some(value.value)
}
