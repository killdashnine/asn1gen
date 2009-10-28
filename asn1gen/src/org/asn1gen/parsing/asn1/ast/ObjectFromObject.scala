package org.asn1gen.parsing.asn1.ast

case class ObjectFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node {
}

