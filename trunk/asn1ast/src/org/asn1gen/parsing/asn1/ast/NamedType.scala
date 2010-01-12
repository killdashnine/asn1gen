package org.asn1gen.parsing.asn1.ast

case class NamedType(
  identifier: Identifier,
  _type: Type
) extends Node with ExtensionAdditionAlternative {
  def name = identifier.name
  def typeKind = _type.kind
  def constraints = _type.constraints
}
