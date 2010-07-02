package org.asn1gen.runtime

import scala.collection.immutable.BitSet

class AsnBitString(value: BitSet) extends AsnType {
  override def _desc: meta.AsnBitString = meta.AsnBitString
  
  def _raw = value
}

object AsnBitString extends AsnBitString(BitSet.empty) {
  def apply(value: BitSet) = if (value.isEmpty) this else new AsnBitString(value)
}
