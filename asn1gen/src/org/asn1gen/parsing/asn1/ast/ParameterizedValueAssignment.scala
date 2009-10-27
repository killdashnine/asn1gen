package org.asn1gen.parsing.asn1.ast

case class ParameterizedValueAssignment(
  valueReference: ValueReference,
  parameterList: ParameterList,
  type_ : Type_,
  value: Value
) extends Node {
}

