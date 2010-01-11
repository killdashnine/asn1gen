package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class TypeConstraint(
  _type: Type
) extends Node with SubtypeElementsKind {
}
