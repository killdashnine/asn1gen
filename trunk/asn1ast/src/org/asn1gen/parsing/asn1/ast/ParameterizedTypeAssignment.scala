package org.asn1gen.parsing.asn1.ast

case class ParameterizedTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  _type: Type
) extends Node with ParameterizedAssignment {
}
