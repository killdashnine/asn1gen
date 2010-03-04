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
  private val minusOne = new AsnInteger(-1)
  private val zero = this
  private val one = new AsnInteger(1)
  private val two = new AsnInteger(2)
  private val three = new AsnInteger(3)
  private val four = new AsnInteger(4)
  private val five = new AsnInteger(5)
  private val six = new AsnInteger(6)
  private val seven = new AsnInteger(7)
  private val eight = new AsnInteger(8)
  private val nine = new AsnInteger(9)
  private val minValue = new AsnInteger(Long.MinValue)
  private val maxValue = new AsnInteger(Long.MaxValue)
  
  def apply(value: Long): AsnInteger = {
    value match {
      case -1 => minusOne
      case 0 => zero
      case 1 => one
      case 2 => two
      case 3 => three
      case 4 => four
      case 5 => five
      case 6 => six
      case 7 => seven
      case 8 => eight
      case 9 => nine
      case x if x == Long.MinValue => minValue
      case x if x == Long.MaxValue => maxValue
      case _ => new AsnInteger(value)
    }
  }
  
  def unapply(value: AsnInteger) = Some(value.value)
}
