package org.asn1gen.parsing.asn1.ast

case class ChoiceValue(
  identifier: Identifier,
  value: Value
) extends Node with BuiltinValue {
}
