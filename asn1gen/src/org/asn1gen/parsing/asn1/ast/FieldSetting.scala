package org.asn1gen.parsing.asn1.ast

case class FieldSetting(
    primitiveFieldName: PrimitiveFieldName,
    setting: Setting
) extends Node {
}

