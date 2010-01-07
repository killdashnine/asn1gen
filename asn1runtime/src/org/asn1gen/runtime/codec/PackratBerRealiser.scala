package org.asn1gen.runtime.codec

trait PackratBerRealiser {
  type AsnBoolean = Boolean
  type AsnInteger = Long
  type AsnNull = org.asn1gen.runtime.AsnNull
  type AsnOctetString = List[Byte]
  type AsnPrintableString = String
  type AsnReal = Double
  
  def mkAsnBoolean(value: Boolean): AsnBoolean = value
  def mkAsnInteger(value: Long): AsnInteger = value
  def mkAsnNull(value: Unit): AsnNull = org.asn1gen.runtime.AsnNull
  def mkAsnOctetString(value: List[Byte]): AsnOctetString = value
  def mkAsnPrintableString(value: List[Byte]): AsnPrintableString = new java.lang.String(value.toArray[Byte])
  def mkAsnReal(value: Double): AsnReal = value
}
