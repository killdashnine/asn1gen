package org.asn1gen.runtime

class AsnInteger(val value: Long) extends AsnType {
  override def _desc: meta.AsnInteger = meta.AsnInteger

  def copy(value: Long = this.value) = AsnInteger(value)

  override def equals(that: Any): Boolean = {
    val other = try {
      that.asInstanceOf[AsnInteger]
    } catch {
      case e: ClassCastException => return false
    }
    this.equals(other: AsnInteger)
  }

  def equals(that: AsnInteger): Boolean = this.value == that.value

  override def hashCode(): Int = this.value.hashCode

  def value(f: (Long => Long)): AsnInteger = this.copy(value = f(this.value))
}

object AsnInteger extends AsnInteger(0) {
  def apply(value: Long): AsnInteger = new AsnInteger(value)
  
  def unapply(): Option[(Long)] = Some(value)
}
