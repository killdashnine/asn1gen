package org.asn1gen.parsing.asn1.ast

case class ObjectSetAssignment(
  objectSetReference: ObjectSetReference,
  definedObjectClass: DefinedObjectClass,
  objectSet: ObjectSet
) extends Node with Assignment {
}
