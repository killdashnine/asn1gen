package org.asn1gen.parsing.asn1.ast

case class FixedTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  type_ : Type_,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpec {
}
