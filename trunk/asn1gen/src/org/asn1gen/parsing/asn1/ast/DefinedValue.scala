package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait DefinedValue
  extends AssignedIdentifier
  with CharsDefn
  with ClassNumber
  with ExceptionIdentification
  with ObjIdComponents
  with NamedNumberValue
  with NumberForm
  with ReferencedValue
  with RelativeOidComponentsKind
  with NamedBitKind {
}
