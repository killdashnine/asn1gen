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
  
  def capitalise: String = value(0).toUpperCase + value.substring(1)
}
