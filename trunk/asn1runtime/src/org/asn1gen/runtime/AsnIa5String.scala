package org.asn1gen.runtime

class AsnIa5String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnIa5String = meta.AsnIa5String
  
  def copy(value: String = this.value) = new AsnIa5String(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnIa5String => this.value == that.value
    case _ => false
  }
}

object AsnIa5String extends AsnIa5String("") {
  def apply(value: String): AsnIa5String = new AsnIa5String(value)
}
