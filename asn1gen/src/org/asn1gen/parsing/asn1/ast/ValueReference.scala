package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ValueReference(chars : String) extends Node with ReferenceKind {
  def name = chars
}

