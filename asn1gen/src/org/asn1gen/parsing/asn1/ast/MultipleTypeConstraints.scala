package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class MultipleTypeConstraints(
  kind: MultipleTypeConstraintsKind
) extends Node with InnerTypeConstraints {
}
