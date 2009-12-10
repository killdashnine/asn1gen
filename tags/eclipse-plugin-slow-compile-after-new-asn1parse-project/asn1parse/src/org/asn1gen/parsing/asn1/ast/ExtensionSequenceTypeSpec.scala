package org.asn1gen.parsing.asn1.ast

case class ExtensionSequenceTypeSpec(
  extensionAndException: ExtensionAndException,
  optionalExtensionMarker: OptionalExtensionMarker
) extends SequenceTypeSpec {
}
