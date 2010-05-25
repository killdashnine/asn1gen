package org.asn1gen.extra

case class ExtraBoolean(value: Boolean) {
  def inspect: String = if (value) "true" else "false"
}
