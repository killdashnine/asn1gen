package org.asn1gen.extra

trait Extras {
  implicit def char2extra(x: Char) = new ExtraChar(x)
}
