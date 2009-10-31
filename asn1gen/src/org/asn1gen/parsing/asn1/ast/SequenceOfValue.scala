package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SequenceOfValue(
  values: List[Value]
) extends Node with BuiltinValueKind {
}
