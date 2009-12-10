package org.asn1gen.parsing.asn1.ast

case class ValueSet(
  elementSetSpecs: ElementSetSpecs
) extends Node
  with ActualParameter
  with GovernorConstraintParameterValue
  with Setting {
}
