package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinedSyntax(
  definedSyntaxTokens: List[DefinedSyntaxToken]
) extends Node with ObjectDefnKind {
}
