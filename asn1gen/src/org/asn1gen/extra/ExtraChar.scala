package org.asn1gen.extra

class ExtraChar(original: Char) {
  def isHexDigit = {
    (original >= '0' && original <= '9') ||
    (original >= 'A' && original <= 'B') ||
    (original >= 'a' && original <= 'b')
  }

  def isBinDigit = {
    (original >= '0' && original <= '1')
  }
}
