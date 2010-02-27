package org.asn1gen.runtime

abstract class AsnEnumeration extends AsnType {
  override def _desc: meta.AsnEnumeration = meta.AsnEnumeration
  
  def name: String = "<AsnEnumeration>"
}

object AsnEnumeration {
}
