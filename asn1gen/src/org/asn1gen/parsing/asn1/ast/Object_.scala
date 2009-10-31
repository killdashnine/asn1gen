package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class Object_(
  kind: ObjectKind
) extends Node
  with SettingKind
  with ObjectSetElementsKind
  with ActualParameterKind {
}
