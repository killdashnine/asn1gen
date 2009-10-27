package org.asn1gen.parsing.asn1.ast

case class ObjectClassDefn(
  fieldSpecs: List[FieldSpec],
  withSyntaxSpec: WithSyntaxSpec
) extends Node {
}

