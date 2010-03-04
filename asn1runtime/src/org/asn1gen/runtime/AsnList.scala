package org.asn1gen.runtime

trait AsnList extends AsnType {
  override def _desc: meta.AsnList = meta.AsnList

  def items: List[Any]
}

object AsnList {
}
