package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Tuple(
  tableColumn: TableColumn,
  tableRow: TableRow
) extends Node with RestrictedCharacterStringValueKind with CharsDefnKind {
}
