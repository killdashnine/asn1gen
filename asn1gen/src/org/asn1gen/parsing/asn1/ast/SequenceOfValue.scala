package org.asn1gen.parsing.asn1.ast

case class SequenceOfValue(
  values: List[Value]
) extends Node with BuiltinValue {
}
