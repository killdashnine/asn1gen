package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedReference(
  reference: Reference,
  hasBraces: Boolean
) extends Node with SymbolKind {
}

