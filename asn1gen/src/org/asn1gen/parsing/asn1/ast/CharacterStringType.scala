package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class CharacterStringType(
  kind: CharacterStringTypeKind
) extends Node with BuiltinType {
}

