package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Number(
  chars: String
) extends Node with NumberFormKind with ClassNumberKind with NumericRealValueKind {
  def negative = Number("-" + chars)
}
