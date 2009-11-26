package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object Empty
  extends Node
  with AssignedIdentifierKind
  with ValueConstraintKind
  with WithSyntaxSpecKind
  with TaggedKind
  with OptionalDefault[Nothing] {
}
