package org.asn1gen.parsing.asn1.ast

case class ValueAssignment(
  valueReference: ValueReference,
  _type: Type,
  value: Value
) extends Node with Assignment {
}
