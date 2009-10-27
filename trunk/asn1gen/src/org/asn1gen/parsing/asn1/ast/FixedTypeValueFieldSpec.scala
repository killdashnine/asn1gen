package org.asn1gen.parsing.asn1.ast

case class FixedTypeValueFieldSpec(
  valueFieldReference: ValueFieldReference,
  type_ : Type_ ,
  unique: Unique,
  valueOptionalitySpec: ValueOptionalitySpec
) extends Node {
}

