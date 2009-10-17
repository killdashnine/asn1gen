package org.asn1gen.extra

class ExtraChar(original: Char) {
  def isUpperHexDigit = {
    (original >= '0' && original <= '9') ||
    (original >= 'A' && original <= 'F')
  }

  def isBinDigit = {
    (original >= '0' && original <= '1')
  }
}
