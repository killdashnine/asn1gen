package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ExternalValueReference(
  moduleReference: ModuleReference,
  valueReference: ValueReference
) extends Node with DefinedValue with SimpleDefinedValueKind {
}
