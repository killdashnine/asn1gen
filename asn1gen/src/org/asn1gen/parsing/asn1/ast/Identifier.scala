package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Identifier(
  chars: String
) extends Node with IntegerValueKind with EnumerationItemKind {
  def name = chars
}
