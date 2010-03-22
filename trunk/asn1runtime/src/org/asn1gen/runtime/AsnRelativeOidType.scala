package org.asn1gen.runtime

// TODO: Interim definition
class AsnRelativeOidType(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnRelativeOidType = meta.AsnRelativeOidType

  def copy(value: String = this.value) = new AsnRelativeOidType(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnRelativeOidType => this.value == that.value
    case _ => false
  }
}

object AsnRelativeOidType extends AsnRelativeOidType("") {
  def apply(value: String): AsnRelativeOidType = new AsnRelativeOidType(value)
}
