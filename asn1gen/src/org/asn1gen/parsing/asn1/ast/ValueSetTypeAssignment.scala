package org.asn1gen.parsing.asn1.ast

case class ValueSetTypeAssignment(
  typeReference: TypeReference,
  _type: Type,
  valueSet: ValueSet
) extends Node with Assignment {
}
