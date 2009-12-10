package org.asn1gen.parsing.asn1.ast

case class ObjectFieldSpec(
  objectFieldReference: ObjectFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectOptionalitySpec: ObjectOptionalitySpec
) extends Node with FieldSpec {
}
