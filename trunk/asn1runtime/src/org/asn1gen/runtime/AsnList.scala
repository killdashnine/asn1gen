package org.asn1gen.runtime

trait AsnList[T <: AsnType] extends AsnType {
  type _Item = T
  
  override def _desc: meta.AsnList = meta.AsnList

  def items: List[T]

  override def equals(that: Any): Boolean = {
    val other = try {
      that.asInstanceOf[AsnList[T]]
    } catch {
      case e: ClassCastException => return false
    }
    this.equals(other: AsnList[T])
  }

  def equals(that: AsnList[T]): Boolean = this.items.sameElements(that.items)

  override def hashCode(): Int = (0 /: this.items)(_ ^ _.hashCode)
}

object AsnList {
}
