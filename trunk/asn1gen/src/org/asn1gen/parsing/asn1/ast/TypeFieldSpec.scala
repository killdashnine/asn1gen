package org.asn1gen.parsing.asn1.ast

case class TypeFieldSpec(
  typeFieldReference: TypeFieldReference,
  typeOptionalitySpec: TypeOptionalitySpec
) extends Node {
}
