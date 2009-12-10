package org.asn1gen.parsing.asn1.ast

case class Identifier(
  chars: String
) extends Node with IntegerValue with EnumerationItem {
  def name = chars
}
