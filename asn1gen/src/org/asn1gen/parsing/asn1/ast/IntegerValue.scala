package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class IntegerValue(
  kind: IntegerValueKind
) extends Node with EnumerationItemKind with BuiltinValueKind {
}
