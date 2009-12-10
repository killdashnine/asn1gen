package org.asn1gen.parsing.asn1.ast

case class ExceptionIdentificationTypeAndValue(
  type_ : Type_,
  value: Value
) extends ExceptionIdentification {
}
