package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ValueFieldReference(
  chars: String
) extends Node with PrimitiveFieldNameKind {
  def name = chars
}

