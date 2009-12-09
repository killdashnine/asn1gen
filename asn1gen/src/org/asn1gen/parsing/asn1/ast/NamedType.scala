package org.asn1gen.parsing.asn1.ast

case class NamedType(
  id: Identifier,
  t: Type_
) extends Node with ExtensionAdditionAlternative {
  def typeName = t.typeName
}
