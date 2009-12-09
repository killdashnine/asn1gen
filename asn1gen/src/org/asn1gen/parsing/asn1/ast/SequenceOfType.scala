package org.asn1gen.parsing.asn1.ast

case class SequenceOfType(
  type_ : Type_
) extends Node with BuiltinType {
  def typeName: String = "<SequenceOfType>"
}
