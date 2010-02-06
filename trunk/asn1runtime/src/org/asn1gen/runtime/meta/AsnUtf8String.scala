package org.asn1gen.runtime.meta

trait AsnUtf8String extends AsnCharacterString {
  override def name: String = "AsnUtf8String"
}

object AsnUtf8String extends AsnUtf8String
