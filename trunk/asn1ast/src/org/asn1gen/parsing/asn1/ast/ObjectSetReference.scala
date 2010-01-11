package org.asn1gen.parsing.asn1.ast

case class ObjectSetReference(
  chars: String
) extends Node with Reference with DefinedObjectSet {
  def name = chars
}
