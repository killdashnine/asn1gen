package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectSetFromObjects(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ObjectSetElementsKind {
}

