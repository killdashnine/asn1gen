package org.asn1gen.parsing.asn1.ast

case class NamedNumber(
  identifier: Identifier,
  value: NamedNumberValue
) extends Node with EnumerationItem {
  def name = identifier.name
}
