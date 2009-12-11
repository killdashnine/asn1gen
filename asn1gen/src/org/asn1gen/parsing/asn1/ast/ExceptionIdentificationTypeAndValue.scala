package org.asn1gen.parsing.asn1.ast

case class ExceptionIdentificationTypeAndValue(
  _type: Type,
  value: Value
) extends ExceptionIdentification {
}
