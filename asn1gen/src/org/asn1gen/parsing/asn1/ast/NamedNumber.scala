package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class NamedNumber(
  identifier: Identifier,
  value: NamedNumberValue
) extends Node with EnumerationItemKind {
  def name = identifier.name
}
