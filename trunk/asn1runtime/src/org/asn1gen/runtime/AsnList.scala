package org.asn1gen.runtime

trait AsnList[T <: AsnType] extends AsnType {
  type _Item = T
  
  override def _desc: meta.AsnList = meta.AsnList

  def items: List[T]

  // TODO: Don't define this here.  Define in derived classes.
  override def equals(that: Any): Boolean = that match {
    case that: AsnList[T] => this.items.sameElements(that.items)
    case _ => false
  }

  override def hashCode(): Int = (0 /: this.items)(_ ^ _.hashCode)
}

object AsnList {
}
