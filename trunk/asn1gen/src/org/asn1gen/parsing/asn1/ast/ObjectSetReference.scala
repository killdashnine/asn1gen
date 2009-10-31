package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectSetReference(
  chars: String
) extends Node with ReferenceKind with DefinedObjectSetKind {
  def name = chars
}

