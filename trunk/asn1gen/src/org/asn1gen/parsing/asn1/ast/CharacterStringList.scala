package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class CharacterStringList(
  charsDefns: List[CharsDefn]
) extends Node with RelativeOidComponentsKind with RestrictedCharacterStringValueKind {
}
