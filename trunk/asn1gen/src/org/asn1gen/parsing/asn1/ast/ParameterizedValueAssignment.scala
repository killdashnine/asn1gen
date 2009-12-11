package org.asn1gen.parsing.asn1.ast

case class ParameterizedValueAssignment(
  valueReference: ValueReference,
  parameterList: ParameterList,
  _type: Type,
  value: Value
) extends Node with ParameterizedAssignment {
}
