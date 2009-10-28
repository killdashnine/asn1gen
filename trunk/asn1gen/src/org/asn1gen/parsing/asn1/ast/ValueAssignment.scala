package org.asn1gen.parsing.asn1.ast

case class ValueAssignment(
  valueReference: ValueReference,
  type_ : Type_,
  value: Value
) extends Node with AssignmentKind {
}
