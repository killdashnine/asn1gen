package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ExternalTypeReference(
  moduleReference: ModuleReference,
  typeReference: TypeReference
) extends Node with DefinedTypeKind with SimpleDefinedTypeKind {
}
