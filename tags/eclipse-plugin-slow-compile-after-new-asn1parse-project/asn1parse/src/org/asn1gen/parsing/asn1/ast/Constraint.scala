package org.asn1gen.parsing.asn1.ast

case class Constraint(
  constraintSpec: ConstraintSpec,
  exceptionSpec: ExceptionSpec
) extends Node with ValueConstraint with ConstraintOrSizeConstraint {
}