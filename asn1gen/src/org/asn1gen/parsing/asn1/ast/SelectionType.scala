package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SelectionType(
  identifier: Identifier,
  _type: Type
) extends Node with ReferencedType {
}
