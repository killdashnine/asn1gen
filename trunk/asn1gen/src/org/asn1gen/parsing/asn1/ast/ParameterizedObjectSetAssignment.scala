package org.asn1gen.parsing.asn1.ast

case class ParameterizedObjectSetAssignment(
  objectSetReference: ObjectSetReference,
  parameterList: ParameterList,
  definedObjectClass: DefinedObjectClass,
  objectSet: ObjectSet
) extends Node {
}

