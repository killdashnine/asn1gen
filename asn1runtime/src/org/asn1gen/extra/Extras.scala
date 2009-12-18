package org.asn1gen.extra

trait Extras {
  implicit def toByteExtra(value: Byte) = ByteExtra(value)
}
