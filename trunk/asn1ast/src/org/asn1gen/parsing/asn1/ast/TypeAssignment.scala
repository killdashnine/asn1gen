package org.asn1gen.parsing.asn1.ast

case class TypeAssignment(
  typeReference: TypeReference,
  _type: Type
) extends Node with Assignment {
  def name = typeReference.name
}
