package org.asn1gen.parsing.asn1.ast

case class UserDefinedConstraint(
  parameters: List[UserDefinedConstraintParameter]
) extends Node with GeneralConstraint {
}
