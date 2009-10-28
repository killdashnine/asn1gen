package org.asn1gen.parsing.asn1.ast

case class ExternalValueReference(
  moduleReference: ModuleReference,
  valueReference: ValueReference
) extends Node {
}

