package org.asn1gen.runtime.meta

// TODO: Interim definition
trait AsnObjectDescriptor extends AsnCharacterString {
  override def name: String = "AsnObjectDescriptor"
}

object AsnObjectDescriptor extends AsnObjectDescriptor
