package org.asn1gen.parsing.asn1.ast

case class ParameterizedValue(
  simpleDefinedValue: SimpleDefinedValue,
  actualParameterList: ActualParameterList
) extends Node {
}
