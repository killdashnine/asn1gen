package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class NamedType(
  id: Identifier,
  t: Type_
) extends Node with ExtensionAdditionAlternativeKind {
}
