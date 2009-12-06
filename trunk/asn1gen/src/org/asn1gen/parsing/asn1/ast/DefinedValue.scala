package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinedValue(
  kind: DefinedValueKind
) extends Node
    with AssignedIdentifierKind
    with CharsDefnKind
    with ClassNumberKind
    with ExceptionIdentification
    with ObjIdComponentsKind
    with NamedNumberValue
    with NumberFormKind
    with ReferencedValueKind
    with RelativeOidComponentsKind
    with NamedBitKind {
}