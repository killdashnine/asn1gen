package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectSetFieldSpec(
  objectSetFieldReference: ObjectSetFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectSetOptionalitySpec: ObjectSetOptionalitySpec
) extends Node with FieldSpecKind {
}
