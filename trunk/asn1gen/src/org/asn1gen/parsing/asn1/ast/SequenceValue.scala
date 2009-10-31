package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SequenceValue(
  namedValues: List[NamedValue]
) extends Node with BuiltinValueKind with NumericRealValueKind {
}
