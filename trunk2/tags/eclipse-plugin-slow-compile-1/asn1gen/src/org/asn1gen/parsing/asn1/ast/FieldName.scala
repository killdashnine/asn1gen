package org.asn1gen.parsing.asn1.ast

case class FieldName(
  primitiveFieldNames: List[PrimitiveFieldName]
) extends Node {
}
