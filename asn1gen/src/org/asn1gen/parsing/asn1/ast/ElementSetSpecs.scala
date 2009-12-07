package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ElementSetSpecs(
  root: RootElementSetSpec,
  additional: Option[Option[AdditionalElementSetSpec]]
) extends Node with ConstraintSpecKind {
}

