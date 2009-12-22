package org.asn1gen.parsing.asn1.ast

case class TypeAssignment(
  name: TypeReference,
  _type: Type
) extends Node with Assignment {
}
