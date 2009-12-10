package org.asn1gen.parsing.asn1.ast

case class ExternalTypeReference(
  moduleReference: ModuleReference,
  typeReference: TypeReference
) extends Node with DefinedType with SimpleDefinedType {
}
