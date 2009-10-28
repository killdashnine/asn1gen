package org.asn1gen.parsing.asn1.ast

case class ObjectClassFieldType(
  definedObjectClass: DefinedObjectClass,
  fieldName: FieldName
) extends Node with BuiltinTypeKind {
}

