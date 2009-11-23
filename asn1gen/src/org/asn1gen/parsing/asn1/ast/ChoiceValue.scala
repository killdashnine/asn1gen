package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ChoiceValue(
  identifier: Identifier,
  value: Value
) extends Node with BuiltinValueKind {
}