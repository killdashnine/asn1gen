package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class UsefulType(
  name: String
) extends Node with ReferencedTypeKind {
}
