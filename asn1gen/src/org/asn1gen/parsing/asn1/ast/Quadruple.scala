package org.asn1gen.parsing.asn1.ast

case class Quadruple(
  group: Group,
  plane: Plane,
  row: Row,
  cell: Cell
) extends Node {
}

