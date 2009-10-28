package org.asn1gen.parsing.asn1.ast

case class ModuleIdentifier(reference : ModuleReference, definitiveIdentifier : DefinitiveIdentifier) extends Node {
  def name = reference.name
}

