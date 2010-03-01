package org.asn1gen.runtime

abstract class AsnEnumeration extends AsnType {
  override def _desc: meta.AsnEnumeration = meta.AsnEnumeration
  
  def _value: Long
  
  def _shortName: Option[String]
}

object AsnEnumeration {
}
