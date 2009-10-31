package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Constraint(
  constraintSpec: ConstraintSpec,
  exceptionSpec: ExceptionSpec
) extends Node with ValueConstraintKind {
}
