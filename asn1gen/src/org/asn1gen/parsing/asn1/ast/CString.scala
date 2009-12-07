package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class CString(
  chars: String
) extends Node
  with RestrictedCharacterStringValue
  with CharsDefnKind {
}
