package org.asn1gen.extra

trait Extras {
  implicit def toExtra(x: Char) = new ExtraChar(x)

  implicit def toExtra(x: String) = new ExtraString(x)

  implicit def toExtra(value: Byte) = ByteExtra(value)
}

object Extras extends Extras