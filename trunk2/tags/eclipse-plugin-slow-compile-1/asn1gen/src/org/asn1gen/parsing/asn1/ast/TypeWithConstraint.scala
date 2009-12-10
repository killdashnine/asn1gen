package org.asn1gen.parsing.asn1.ast

case class TypeWithConstraint(
  typeSpec: SequenceOrSet,
  constraintSpec: ConstraintOrSizeConstraint,
  type_ : Type_
) extends Node {
}

