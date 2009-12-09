package org.asn1gen.parsing.asn1.ast

case class ValueSetFromObjects(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ReferencedType {
  def typeName: String = "<ValueSetFromObjects>"
}
