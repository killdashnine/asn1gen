package org.asn1gen.runtime

class AsnGraphicString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnGraphicString = meta.AsnGraphicString

  def copy(value: String = this.value) = new AsnGraphicString(value)

  override def equals(that: Any): Boolean = that match {
    case that: AsnGraphicString => this.value == that.value
    case _ => false
  }
}

object AsnGraphicString extends AsnGraphicString("") {
  def apply(value: String): AsnGraphicString = new AsnGraphicString(value)
}
