package org.asn1gen.parsing.asn1.ast

case class ValueSetTypeAssignment(
  typeReference: TypeReference,
  type_ : Type_,
  valueSet: ValueSet
) extends Node with AssignmentKind {
}

