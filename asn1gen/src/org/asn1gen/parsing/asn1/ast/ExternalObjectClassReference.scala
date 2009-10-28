package org.asn1gen.parsing.asn1.ast

case class ExternalObjectClassReference(
  moduleReference: ModuleReference,
  objectClassReference: ObjectClassReference
) extends Node {
}
