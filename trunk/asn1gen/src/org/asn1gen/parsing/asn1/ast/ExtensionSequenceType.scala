package org.asn1gen.parsing.asn1.ast

case class ExtensionSequenceType(
  extensionAndException: ExtensionAndException,
  optionalExtensionMarker: OptionalExtensionMarker
) extends SequenceTypeSpec {
}
