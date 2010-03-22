package org.asn1gen.runtime

// TODO: Interim definition
class AsnObjectDescriptor(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnObjectDescriptor = meta.AsnObjectDescriptor

  def copy(value: String = this.value) = new AsnObjectDescriptor(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnObjectDescriptor => this.value == that.value
    case _ => false
  }
}

object AsnObjectDescriptor extends AsnObjectDescriptor("") {
  def apply(value: String): AsnObjectDescriptor = new AsnObjectDescriptor(value)
}
