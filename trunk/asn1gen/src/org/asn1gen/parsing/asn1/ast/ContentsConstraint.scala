package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ContentsConstraint(
  type_ : Option[Type_],
  value: Option[Value]
) extends Node with GeneralConstraintKind {
}

