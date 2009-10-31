package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedObjectAssignment(
  objectReference: ObjectReference,
  parameterList: ParameterList,
  definedObjectClass: DefinedObjectClass,
  object_ : Object_
) extends Node with ParameterizedAssignmentKind {
}
