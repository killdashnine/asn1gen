package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedObjectSetAssignment(
  objectSetReference: ObjectSetReference,
  parameterList: ParameterList,
  definedObjectClass: DefinedObjectClass,
  objectSet: ObjectSet
) extends Node with ParameterizedAssignmentKind {
}
