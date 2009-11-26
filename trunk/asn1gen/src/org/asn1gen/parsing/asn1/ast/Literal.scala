package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Literal(
  word: Option[Word]
) extends Node with RequiredTokenKind with DefinedSyntaxTokenKind {
}
