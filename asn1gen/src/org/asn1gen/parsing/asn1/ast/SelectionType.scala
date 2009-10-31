package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SelectionType(
  identifier: Identifier,
  type_ : Type_
) extends Node with ReferencedTypeKind {
}
