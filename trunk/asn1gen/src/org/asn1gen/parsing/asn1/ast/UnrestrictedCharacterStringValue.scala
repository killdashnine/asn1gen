package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class UnrestrictedCharacterStringValue(
  sequenceValue: SequenceValue
) extends Node with CharacterStringValueKind {
}


