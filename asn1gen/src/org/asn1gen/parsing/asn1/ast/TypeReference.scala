package org.asn1gen.parsing.asn1.ast

case class TypeReference(chars : String) extends Node {
  def name = chars
  def asModuleReference = ModuleReference(chars)
}

