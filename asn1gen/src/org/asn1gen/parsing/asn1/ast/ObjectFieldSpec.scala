package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectFieldSpec(
  objectFieldReference: ObjectFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectOptionalitySpec: ObjectOptionalitySpec
) extends Node with FieldSpecKind {
}
