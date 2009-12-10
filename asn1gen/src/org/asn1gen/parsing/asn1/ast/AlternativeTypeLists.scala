package org.asn1gen.parsing.asn1.ast

case class AlternativeTypeLists(
  typeList: RootAlternativeTypeList,
  extensionAndException: Option[ExtensionAndException],
  extensionAdditionAlternatives: Option[ExtensionAdditionAlternatives],
  optionalExtensionMarker: Option[OptionalExtensionMarker]) extends Node {
}

