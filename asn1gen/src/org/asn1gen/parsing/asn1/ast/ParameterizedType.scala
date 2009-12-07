package org.asn1gen.parsing.asn1.ast

case class ParameterizedType(
  simpleDefinedType: SimpleDefinedType,
  actualParameterList: ActualParameterList
) extends Node with DefinedType {
}

