package org.asn1gen.runtime.codec

trait PackratBerRealiser {
  type AsnBoolean = Boolean
  type AsnInteger = Long
  type AsnNull = Unit
  type AsnOctetString = List[Byte]
  type AsnReal = Double
  
  def mkAsnBoolean(value: Boolean): AsnBoolean = value
  def mkAsnInteger(value: Long): AsnInteger = value
  def mkAsnNull(value: Unit): AsnNull = value
  def mkAsnOctetString(value: List[Byte]): AsnOctetString = value
  def mkAsnReal(value: Double): AsnReal = value
}
