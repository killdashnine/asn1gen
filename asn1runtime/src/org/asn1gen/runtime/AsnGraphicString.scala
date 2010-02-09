package org.asn1gen.runtime

class AsnGraphicString(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnGraphicString = meta.AsnGraphicString

  def copy(value: String = this.value) = new AsnGraphicString(value)

  def equals(that: AsnGraphicString) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnGraphicString].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnGraphicString {
  def apply(value: String): AsnGraphicString = new AsnGraphicString(value)
}
