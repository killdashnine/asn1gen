package org.asn1gen.parsing.asn1.ast

case class TypeFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node {
}

