package org.asn1gen.runtime.meta

trait AsnSequence extends AsnType {
  override def name: String = "AsnSequence"
}

object AsnSequence extends AsnSequence
