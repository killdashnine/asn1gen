package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectClassReference(
  chars: String
) extends Node with ReferenceKind with DefinedObjectClassKind {
  def name = chars
}
