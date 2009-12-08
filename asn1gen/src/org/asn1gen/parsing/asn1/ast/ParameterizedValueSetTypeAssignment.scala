package org.asn1gen.parsing.asn1.ast

case class ParameterizedValueSetTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  type_ : Type_,
  valueSet: ValueSet
) extends Node with ParameterizedAssignment {
}
