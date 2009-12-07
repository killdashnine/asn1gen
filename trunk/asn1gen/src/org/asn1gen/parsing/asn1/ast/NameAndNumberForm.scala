package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class NameAndNumberForm(
  identifier: Identifier,
  numberForm: NumberForm
) extends Node with ObjIdComponents with RelativeOidComponentsKind {
}
