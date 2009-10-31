package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Quadruple(
  group: Group,
  plane: Plane,
  row: Row,
  cell: Cell
) extends Node with RelativeOidComponentsKind with CharsDefnKind with RestrictedCharacterStringValueKind {
}
