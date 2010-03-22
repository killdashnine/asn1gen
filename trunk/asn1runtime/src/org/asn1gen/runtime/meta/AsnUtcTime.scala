package org.asn1gen.runtime.meta

// TODO: Interim definition
trait AsnUtcTime extends AsnCharacterString {
  override def name: String = "AsnUtcTime"
}

object AsnUtcTime extends AsnUtcTime
