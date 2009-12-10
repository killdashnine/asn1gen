package org.asn1gen.parsing.asn1.ast

case class ElementSetSpecs(
  root: RootElementSetSpec,
  additional: Option[Option[AdditionalElementSetSpec]]
) extends Node with ConstraintSpec {
}

