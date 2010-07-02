package org.asn1gen.runtime

sealed class AsnBoolean(val value: Boolean) extends AsnType {
  override def _desc: meta.AsnBoolean = meta.AsnBoolean
  
  def _raw = value

  def copy(value: Boolean = this.value): AsnBoolean = AsnBoolean(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnBoolean => this.value == that.value
    case _ => false
  }

  override def hashCode(): Int = this.value.hashCode

  def value(f: (Boolean => Boolean)): AsnBoolean = if (f(this.value)) AsnTrue else AsnFalse
}

object AsnBoolean extends AsnBoolean(false) {
  def apply(value: Boolean) = if (value) AsnTrue else AsnFalse

  def unapply(value: AsnBoolean) = Some(value.value)
}

object AsnFalse extends AsnBoolean(false) {
}

object AsnTrue extends AsnBoolean(true) {
}
