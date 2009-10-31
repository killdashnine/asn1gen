package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ExternalObjectSetReference(
  moduleReference: ModuleReference,
  objectSetReference: ObjectSetReference
) extends Node with DefinedObjectSetKind {
}
