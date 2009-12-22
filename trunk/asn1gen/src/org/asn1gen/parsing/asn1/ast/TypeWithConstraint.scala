package org.asn1gen.parsing.asn1.ast

case class TypeWithConstraint(
  typeSpec: SequenceOrSet,
  constraintSpec: ConstraintOrSizeConstraint,
  _type: Type
) extends Node {
}
