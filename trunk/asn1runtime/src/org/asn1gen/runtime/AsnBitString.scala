package org.asn1gen.runtime

import scala.collection.immutable.BitSet

class AsnBitString extends AsnType {
  override def _desc: meta.AsnBitString = meta.AsnBitString
}

object AsnBitString extends AsnBitString {
}
