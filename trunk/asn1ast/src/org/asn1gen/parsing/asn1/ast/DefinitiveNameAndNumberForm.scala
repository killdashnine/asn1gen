package org.asn1gen.parsing.asn1.ast

case class DefinitiveNameAndNumberForm(
  identifier: Identifier,
  definitiveNumberForm: DefinitiveNumberForm
) extends Node with DefinitiveObjectIdComponent {
}
