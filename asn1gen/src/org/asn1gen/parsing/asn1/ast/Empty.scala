package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object Empty
  extends Node
  with AssignedIdentifierKind
  with Class_
  with PresenceConstraint
  with ValueConstraintKind
  with WithSyntaxSpecKind
  with TaggedKind
  with OptionalDefault[Nothing]
  with TagDefaultKind {
}
