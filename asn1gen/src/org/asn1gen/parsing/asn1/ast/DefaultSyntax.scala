package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class DefaultSyntax(
  fieldSettings: List[FieldSetting]
) extends Node with ObjectDefnKind {
}
