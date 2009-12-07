package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class NameForm(
  identifier: Identifier
) extends Node
  with DefinitiveObjectIdComponent
  with ObjIdComponentsKind {
}
