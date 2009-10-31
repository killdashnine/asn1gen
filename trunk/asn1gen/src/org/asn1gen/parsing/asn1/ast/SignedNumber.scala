package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SignedNumber(
  negative: Boolean,
  magnitude: Number
) extends Node with NamedNumberValue with IntegerValueKind {
}

