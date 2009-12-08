package org.asn1gen.parsing.asn1.ast

case class ParameterizedObjectClassAssignment(
  objectClassReference: ObjectClassReference,
  parameterList: ParameterList,
  objectClass: ObjectClass
) extends Node with ParameterizedAssignment {
}
