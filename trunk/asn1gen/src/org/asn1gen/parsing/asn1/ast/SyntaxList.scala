package org.asn1gen.parsing.asn1.ast

case class SyntaxList(
  tokenOrGroupSpecs: List[TokenOrGroupSpec]
) extends Node with WithSyntaxSpec {
}
