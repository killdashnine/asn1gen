package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class RestrictedCharacterStringValue(
  kind: RestrictedCharacterStringValueKind
) extends Node with CharacterStringValueKind {
}
