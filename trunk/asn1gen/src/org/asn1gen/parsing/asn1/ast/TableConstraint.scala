package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class TableConstraint(
  kind: TableConstraintKind
) extends Node with GeneralConstraintKind {
}

