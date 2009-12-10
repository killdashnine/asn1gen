package org.asn1gen.parsing.asn1.ast

case class ObjectAssignment(
  objectReference: ObjectReference,
  definedObjectClass: DefinedObjectClass,
  object_ : Object_
) extends Node with Assignment {
}
