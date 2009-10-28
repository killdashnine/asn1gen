package org.asn1gen.parsing.asn1.ast

case class VariableTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  fieldName: FieldName,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node {
}
