package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedObject(
  definedObject: DefinedObject,
  actualParameterList: ActualParameterList
) extends Node with ObjectKind with ReferencedObjectsKind {
}
