package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Value(
  kind: ValueKind
) extends Node
  with ActualParameterKind
  with GovernorConstraintParameterValue
  with LowerEndValueKind
  with SettingKind
  with UpperEndValueKind {
}
