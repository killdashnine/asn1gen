package org.asn1gen.parsing.asn1.ast

case class EnumeratedType(
  enumerations: Enumerations
) extends Node with BuiltinType {
}
