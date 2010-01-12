package org.asn1gen.parsing.asn1.ast

case class NamedComponentType(
  namedType: NamedType,
  value: OptionalDefault[Value]
) extends ComponentType {
  def name = namedType.name
}
