package org.asn1gen.parsing.asn1.ast

case class ActualParameter() extends Node {
}

case class ActualParameterList() extends Node {
}

case class AdditionalElementSetSpec() extends Node {
}

case class AdditionalEnumeration() extends Node {
}

case class AlternativeTypeList() extends Node {
}

case class AlternativeTypeLists() extends Node {
}

case class AssignedIdentifier() extends Node {
}

case class Assignment() extends Node {
}

case class AssignmentList(assignments: List[Assignment]) extends Node {
}

case class AtNotation() extends Node {
}

case class BString(chars : String) extends Node {
}

case class BitStringType() extends Node {
}

case class BitStringValue() extends Node {
}

case class BooleanType() extends Node {
}

case class BooleanValue(value: Boolean) extends Node {
}

case class BuiltinType() extends Node {
}

case class BuiltinValue() extends Node {
}

case class Comment() extends Node {
}

case class CString(chars : String) extends Node {
}

case class Cell() extends Node {
}

case class CharacterStringList() extends Node {
}

case class CharacterStringType() extends Node {
}

case class CharacterStringValue() extends Node {
}

case class CharsDefn() extends Node {
}

case class ChoiceType() extends Node {
}

case class ChoiceValue() extends Node {
}

case class ClassNumber() extends Node {
}

case class Class_() extends Node {
}

case class ComponentConstraint() extends Node {
}

case class ComponentIdList() extends Node {
}

case class ComponentRelationConstraint() extends Node {
}

case class ComponentType() extends Node {
}

case class ComponentTypeList() extends Node {
}

case class ComponentTypeLists() extends Node {
}

case class ConstrainedType() extends Node {
}

case class Constraint() extends Node {
}

case class ConstraintSpec() extends Node {
}

case class ContainedSubtype() extends Node {
}

case class ContentsConstraint() extends Node {
}
