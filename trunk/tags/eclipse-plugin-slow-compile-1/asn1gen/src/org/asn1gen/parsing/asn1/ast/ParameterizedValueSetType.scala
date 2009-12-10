package org.asn1gen.parsing.asn1.ast

case class ParameterizedValueSetType(
  simpleDefinedType: SimpleDefinedType,
  actualParameterList: ActualParameterList
) extends Node with DefinedType {
  def typeName: String = "<ParameterizedValueSetType>"
}
