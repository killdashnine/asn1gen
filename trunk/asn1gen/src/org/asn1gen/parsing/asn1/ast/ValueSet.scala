package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ValueSet(
  elementSetSpecs: ElementSetSpecs
) extends Node with SettingKind with ActualParameterKind {
}
