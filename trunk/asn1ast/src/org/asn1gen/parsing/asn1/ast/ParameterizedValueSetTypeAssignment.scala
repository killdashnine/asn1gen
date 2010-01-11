package org.asn1gen.parsing.asn1.ast

case class ParameterizedValueSetTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  _type: Type,
  valueSet: ValueSet
) extends Node with ParameterizedAssignment {
}
