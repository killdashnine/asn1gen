package org.asn1gen.parsing.asn1.ast

case class FullSpecification(
  typeConstraints: TypeConstraints
) extends Node with MultipleTypeConstraints {
}
