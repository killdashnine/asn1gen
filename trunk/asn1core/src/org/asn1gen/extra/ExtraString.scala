package org.asn1gen.extra

class ExtraString(value: String) {
  def inspect(): String = {
    val newValue =
      ( value
      . replace("\"", "\\\"")
      . replace("\n", "\\n")
      . replace("\r", "\\r")
      . replace("\t", "\\t")
      )
    return "\"" + newValue + "\""
  }
  
  def capitalise: String = value(0).toUpper + value.substring(1)
  
  def bin: Int = {
    require(0 < value.length, "String length too small for binary")
    require(value.length < 32, "String length too large for 32-bit binary")

    value.foldLeft(0) { case (n, c) =>
      c match {
        case '0' => n << 1
        case '1' => (n << 1) + 1
        case _ => throw new Exception("")
      }
    }
  }
}
