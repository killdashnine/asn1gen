package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedObjectSet(
  definedObjectSet: DefinedObjectSet,
  actualParameterList: ActualParameterList
) extends Node with ObjectSetElementsKind with ReferencedObjects {
}
