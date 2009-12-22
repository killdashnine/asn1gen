package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Type(
  kind: TypeKind,
  constraints: List[Constraint]
) extends Node
    with ActualParameter
    with GovernorKind
    with Setting
    with UserDefinedConstraintParameter {
}
