package org.asn1gen.parsing.asn1.ast

case class TypeReference(
  name: String
) extends Node
    with Reference
    with DefinedType
    with SimpleDefinedType {
  def asModuleReference = ModuleReference(name)
}
