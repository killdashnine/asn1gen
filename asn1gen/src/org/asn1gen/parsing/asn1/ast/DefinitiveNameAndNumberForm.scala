package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinitiveNameAndNumberForm(
  identifier: Identifier,
  definitiveNumberForm: DefinitiveNumberForm
) extends Node with DefinitiveObjectIdComponentKind {
}

