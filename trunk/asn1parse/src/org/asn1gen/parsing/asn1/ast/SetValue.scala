package org.asn1gen.parsing.asn1.ast

case class SetValue(
  namedValues: List[NamedValue]
) extends Node with BuiltinValue {
}
