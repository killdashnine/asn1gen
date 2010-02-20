package org.asn1gen.extra

case class ExtraByte(value: Byte) {
  def definesBit(index: Int): Boolean = ((1 << index) & value) != 0
}
