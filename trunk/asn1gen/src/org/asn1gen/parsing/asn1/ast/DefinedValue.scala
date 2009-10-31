package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinedValue(
  kind: DefinedValueKind
) extends Node
    with NamedNumberValue
    with ReferencedValueKind
    with AssignedIdentifierKind
    with ObjIdComponentsKind
    with NumberFormKind
    with RelativeOidComponentsKind
    with CharsDefnKind
    with ClassNumberKind
    with NamedBitKind {
}
