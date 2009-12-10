package org.asn1gen.parsing.asn1.ast

case class TypeReference(
  chars: String
) extends Node
    with Reference
    with DefinedType
    with SimpleDefinedType {
  def name = chars
  def asModuleReference = ModuleReference(chars)
}
