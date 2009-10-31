package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectClassDefn(
  fieldSpecs: List[FieldSpec],
  withSyntaxSpec: WithSyntaxSpec
) extends Node with ObjectClassKind {
}

