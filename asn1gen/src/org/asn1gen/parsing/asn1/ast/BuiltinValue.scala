package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class BuiltinValue(
  kind: BuiltinValueKind
) extends Node
  with ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with ValueKind {
}
