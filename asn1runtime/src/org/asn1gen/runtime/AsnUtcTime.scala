package org.asn1gen.runtime

// TODO: Interim definition
class AsnUtcTime(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnUtcTime = meta.AsnUtcTime

  def copy(value: String = this.value) = new AsnUtcTime(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnUtcTime => this.value == that.value
    case _ => false
  }
}

object AsnUtcTime extends AsnUtcTime("") {
  def apply(value: String): AsnUtcTime = new AsnUtcTime(value)
}
