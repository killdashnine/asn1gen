package org.asn1gen.parsing.asn1.ast

case class ParameterizedTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  type_ : Type_ 
) extends Node {
}

