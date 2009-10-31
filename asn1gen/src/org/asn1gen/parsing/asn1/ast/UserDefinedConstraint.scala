package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class UserDefinedConstraint(
  parameters: List[UserDefinedConstraintParameter]
) extends Node with GeneralConstraintKind {
}
