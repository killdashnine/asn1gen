package org.asn1gen.parsing.asn1.ast

case class FixedTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  _type: Type,
  unique: Option[UNIQUE],
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node with FieldSpec{
}
