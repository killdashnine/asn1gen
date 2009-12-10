package org.asn1gen.parsing.asn1.ast

case class GovernorConstraintParameter(
  governor: Governor,
  value: GovernorConstraintParameterValue
) extends UserDefinedConstraintParameter {
}
