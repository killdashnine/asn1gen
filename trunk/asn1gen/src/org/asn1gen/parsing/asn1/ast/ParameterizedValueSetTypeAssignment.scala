package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedValueSetTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  type_ : Type_,
  valueSet: ValueSet
) extends Node with ParameterizedAssignmentKind {
}
