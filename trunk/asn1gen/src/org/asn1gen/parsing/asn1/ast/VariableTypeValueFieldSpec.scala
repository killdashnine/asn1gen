package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class VariableTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  fieldName: FieldName,
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node with FieldSpecKind {
}

