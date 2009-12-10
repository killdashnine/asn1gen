package org.asn1gen.parsing.asn1.ast

case class ObjectReference(
  chars: String
) extends Node with Reference with DefinedObject {
  def name = chars
}
