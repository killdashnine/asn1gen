package org.asn1gen.parsing.asn1.ast

case class Tuple(
  tableColumn: TableColumn,
  tableRow: TableRow
) extends Node with RestrictedCharacterStringValue with CharsDefn {
}
