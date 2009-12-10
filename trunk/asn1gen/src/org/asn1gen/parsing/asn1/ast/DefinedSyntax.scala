package org.asn1gen.parsing.asn1.ast

case class DefinedSyntax(
  definedSyntaxTokens: List[DefinedSyntaxToken]
) extends Node with ObjectDefn {
}
