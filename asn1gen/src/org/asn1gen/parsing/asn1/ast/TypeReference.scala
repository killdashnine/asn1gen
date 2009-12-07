package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class TypeReference(
  chars: String
) extends Node
    with Reference
    with DefinedTypeKind
    with SimpleDefinedTypeKind {
  def name = chars
  def asModuleReference = ModuleReference(chars)
}
