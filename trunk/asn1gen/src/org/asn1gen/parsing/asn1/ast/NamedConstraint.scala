package org.asn1gen.parsing.asn1.ast

case class NamedConstraint(
  identifier: Identifier,
  componentConstraint: ComponentConstraint
) extends Node {
}

