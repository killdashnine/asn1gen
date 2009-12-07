package org.asn1gen.parsing.asn1.ast

case class ParameterizedReference(
  reference: Reference,
  hasBraces: Boolean
) extends Node with Symbol {
}
