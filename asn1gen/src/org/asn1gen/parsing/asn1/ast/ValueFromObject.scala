package org.asn1gen.parsing.asn1.ast

case class ValueFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ReferencedValueKind {
}

