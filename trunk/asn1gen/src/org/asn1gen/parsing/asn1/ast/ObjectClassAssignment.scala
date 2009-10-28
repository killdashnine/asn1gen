package org.asn1gen.parsing.asn1.ast

case class ObjectClassAssignment(
  objectClassReference: ObjectClassReference,
  objectClass: ObjectClass
) extends Node with AssignmentKind {
}

