package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefinedValue() extends Node
  with NamedNumberValue
  with ReferencedValueKind
  with AssignedIdentifierKind
{
}
