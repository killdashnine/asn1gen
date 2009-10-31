package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class VariableTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  fieldName: FieldName,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpecKind {
}
