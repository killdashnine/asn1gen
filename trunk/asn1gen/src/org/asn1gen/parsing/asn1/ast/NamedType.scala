package org.asn1gen.parsing.asn1.ast

case class NamedType(
  id: Identifier,
  _type: Type
) extends Node with ExtensionAdditionAlternative {
}
