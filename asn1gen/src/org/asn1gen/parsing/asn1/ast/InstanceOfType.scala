package org.asn1gen.parsing.asn1.ast

case class InstanceOfType(
  definedObjectClass: DefinedObjectClass
) extends Node with BuiltinType {
  def typeName: String = "<InstanceOfType>"
}
