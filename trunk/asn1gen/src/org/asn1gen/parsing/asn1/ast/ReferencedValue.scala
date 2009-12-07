package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ReferencedValue(
  kind: ReferencedValueKind
) extends Node
  with ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with Value {
}
