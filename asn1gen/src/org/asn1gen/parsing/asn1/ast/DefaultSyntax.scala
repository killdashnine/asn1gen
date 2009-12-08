package org.asn1gen.parsing.asn1.ast

case class DefaultSyntax(
  fieldSettings: List[FieldSetting]
) extends Node with ObjectDefn {
}
