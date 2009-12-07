package org.asn1gen.parsing.asn1.ast

case class SignedNumber(
  negative: Boolean,
  magnitude: Number
) extends Node
    with NamedNumberValue
    with IntegerValue
    with ExceptionIdentification {
}
