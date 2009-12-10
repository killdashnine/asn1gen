package org.asn1gen.parsing.asn1.ast

case class NamedValue(
  identifier: Identifier,
  value: Value
) extends Node {
}

