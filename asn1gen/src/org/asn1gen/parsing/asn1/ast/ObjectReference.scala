package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectReference(
  chars: String
) extends Node with Reference with DefinedObjectKind {
  def name = chars
}
