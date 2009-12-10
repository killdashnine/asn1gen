package org.asn1gen.parsing.asn1.ast

case class ExternalObjectSetReference(
  moduleReference: ModuleReference,
  objectSetReference: ObjectSetReference
) extends Node with DefinedObjectSet {
}
