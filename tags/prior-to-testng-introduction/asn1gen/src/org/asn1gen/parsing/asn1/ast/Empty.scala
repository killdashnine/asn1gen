package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object Empty
  extends Node
  with AssignedIdentifier
  with Class_
  with ExceptionSpec
  with ExtensionAdditionAlternatives
  with OptionalDefault[Nothing]
  with PresenceConstraint
  with SequenceTypeSpec
  with TaggedKind
  with TagDefault
  with ValueConstraint
  with WithSyntaxSpec {
}
