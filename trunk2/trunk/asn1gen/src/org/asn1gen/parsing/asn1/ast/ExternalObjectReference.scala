package org.asn1gen.parsing.asn1.ast

case class ExternalObjectReference(
  moduleReference: ModuleReference,
  objectReference: ObjectReference
) extends Node with DefinedObject {
}
