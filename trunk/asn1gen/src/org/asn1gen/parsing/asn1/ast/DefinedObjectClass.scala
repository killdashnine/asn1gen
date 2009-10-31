package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinedObjectClass(
  kind: DefinedObjectClassKind
) extends Node
  with GovernorKind
  with ObjectClassKind
  with ActualParameterKind {
}
