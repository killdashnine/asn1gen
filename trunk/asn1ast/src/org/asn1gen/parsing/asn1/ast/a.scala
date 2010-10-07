package org.asn1gen.parsing.asn1.ast

object ABSTRACT_SYNTAX extends UsefulObjectClassReference {
}

object Absent extends PresenceConstraint {
}

trait ActualParameter {
}

case class ActualParameterList(parameters: List[ActualParameter]) extends Node {
}

case class AdditionalElementSetSpec(elementSetSpec: ElementSetSpec) extends Node {
}

case class AdditionalEnumeration(
  enumeration: Enumeration
) extends Node {
}

case class AlternativeTypeList(namedTypes: List[NamedType]) extends Node {
}

case class AlternativeTypeLists(
  typeList: RootAlternativeTypeList,
  extensionAndException: Option[ExtensionAndException],
  extensionAdditionAlternatives: Option[ExtensionAdditionAlternatives],
  optionalExtensionMarker: Option[OptionalExtensionMarker]) extends Node {
}

object Application extends Class_ {
}

trait AssignedIdentifier {
}

trait Assignment {
}

case class AssignmentList(assignments: List[Assignment]) extends Node {
}

case class AtNotation(
  componentIdList: ComponentIdList,
  hasDot: Boolean
) extends Node {
}

object Automatic extends Node  with TagDefault {
}
