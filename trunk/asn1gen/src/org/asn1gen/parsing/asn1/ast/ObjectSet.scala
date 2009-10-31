package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectSet(
  objectSetSpec: ObjectSetSpec
) extends Node with SettingKind with ActualParameterKind {
}
