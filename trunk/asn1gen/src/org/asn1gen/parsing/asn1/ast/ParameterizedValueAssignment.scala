package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedValueAssignment(
  valueReference: ValueReference,
  parameterList: ParameterList,
  type_ : Type_,
  value: Value
) extends Node with ParameterizedAssignmentKind {
}
