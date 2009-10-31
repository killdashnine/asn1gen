package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class FixedTypeValueSetFieldSpec(
  valueSetFieldReference: ValueSetFieldReference,
  type_ : Type_,
  valueSetOptionalitySpec: ValueSetOptionalitySpec
) extends Node with FieldSpecKind {
}
