package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SetOfValue(
  values: List[Value]
) extends Node with BuiltinValueKind {
}
