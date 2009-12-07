package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Type_(
  kind: TypeKind,
  constraints: List[Constraint]
) extends Node
  with ActualParameterKind
  with GovernorKind
  with SettingKind
  with UserDefinedConstraintParameter {
}
