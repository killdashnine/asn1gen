package org.asn1gen.parsing.asn1.ast

case class FixedTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  _type: Type,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpec {
}
