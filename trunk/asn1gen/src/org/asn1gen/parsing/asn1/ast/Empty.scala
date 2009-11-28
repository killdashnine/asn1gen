package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object Empty
  extends Node
  with AssignedIdentifierKind
  with Class_
  with ExceptionSpecKind
  with OptionalDefault[Nothing]
  with PresenceConstraint
  with TaggedKind
  with TagDefaultKind
  with ValueConstraintKind
  with WithSyntaxSpecKind {
}
