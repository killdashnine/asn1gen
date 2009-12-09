package org.asn1gen.parsing.asn1.ast

case class OptionalGroup(
  tokenOrGroupSpecs: List[TokenOrGroupSpec]
) extends Node with TokenOrGroupSpec {
}
