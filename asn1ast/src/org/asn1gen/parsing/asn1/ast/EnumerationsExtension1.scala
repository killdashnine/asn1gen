package org.asn1gen.parsing.asn1.ast

case class EnumerationsExtension1(
  exceptionSpec: ExceptionSpec,
  extension: Option[EnumerationsExtension2]
) extends Node {
}
