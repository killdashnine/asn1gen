package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class FixedTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  type_ : Type_ ,
  unique: Option[UNIQUE],
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node with FieldSpecKind {
}
