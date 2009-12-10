package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ObjectKind {
}

