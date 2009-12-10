package org.asn1gen.parsing.asn1.ast

case class ExternalValue(
  sequenceValue: SequenceValue
) extends Node with BuiltinValue {
}
