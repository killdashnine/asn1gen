package org.asn1gen.parsing.asn1.ast

case class ConstrainedType(
  typeWithConstraint: TypeWithConstraint
) extends Node with TypeKind {
  def typeName: String = "<ConstrainedType>"
}
