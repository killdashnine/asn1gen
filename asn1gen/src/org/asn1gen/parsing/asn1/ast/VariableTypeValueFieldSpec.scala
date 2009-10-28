package org.asn1gen.parsing.asn1.ast

case class VariableTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  fieldName: FieldName,
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node {
}

