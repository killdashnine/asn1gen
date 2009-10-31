package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class OptionalGroup(
  tokenOrGroupSpecs: List[TokenOrGroupSpec]
) extends Node with TokenOrGroupSpecKind {
}
