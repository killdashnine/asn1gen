package org.asn1gen.parsing.asn1.ast

case class Identifier(
  name: String
) extends Node with IntegerValue with EnumerationItem {
}
