package org.asn1gen.parsing.asn1.ast

case class ObjectSetFieldSpec(
  objectSetFieldReference: ObjectSetFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectSetOptionalitySpec: ObjectSetOptionalitySpec
) extends Node {
}

