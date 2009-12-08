package org.asn1gen.parsing.asn1.ast

case class Literal(
  word: Option[Word]
) extends Node with RequiredToken with DefinedSyntaxToken {
}
