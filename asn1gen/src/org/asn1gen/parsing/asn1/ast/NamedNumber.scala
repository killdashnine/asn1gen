package org.asn1gen.parsing.asn1.ast

case class NamedNumber(identifier: Identifier, value: NamedNumberValue) extends Node {
  def name = identifier.name
}

