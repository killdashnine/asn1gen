package org.asn1gen.runtime

class AsnUtf8String(value: String) extends AsnCharacterString(value) {
  override def _desc: meta.AsnUtf8String = meta.AsnUtf8String

  def copy(value: String = this.value) = new AsnUtf8String(value)

  def equals(that: AsnUtf8String) = this.value == that.value

  override def equals(that: Any): Boolean = {
    try {
      return that.asInstanceOf[AsnUtf8String].equals(this)
    } catch {
      case e: ClassCastException => return false
    }
  }
}

object AsnUtf8String extends AsnUtf8String("") {
  def apply(value: String): AsnUtf8String = new AsnUtf8String(value)
}
