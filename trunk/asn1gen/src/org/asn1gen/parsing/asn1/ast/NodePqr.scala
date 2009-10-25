package org.asn1gen.parsing.asn1.ast

case class ParamGovernor() extends Node {
}

case class Parameter() extends Node {
}

case class ParameterList() extends Node {
}

case class ParameterizedAssignment() extends Node with AssignmentKind {
}

case class ParameterizedObject() extends Node {
}

case class ParameterizedObjectAssignment() extends Node {
}

case class ParameterizedObjectClass() extends Node {
}

case class ParameterizedObjectClassAssignment() extends Node {
}

case class ParameterizedObjectSet() extends Node {
}

case class ParameterizedObjectSetAssignment() extends Node {
}

case class ParameterizedReference() extends Node {
}

case class ParameterizedType() extends Node {
}

case class ParameterizedTypeAssignment() extends Node {
}

case class ParameterizedValue() extends Node {
}

case class ParameterizedValueAssignment() extends Node {
}

case class ParameterizedValueSetType() extends Node {
}

case class ParameterizedValueSetTypeAssignment() extends Node {
}

case class PartialSpecification() extends Node {
}

case class PatternConstraint() extends Node {
}

case class PermittedAlphabet() extends Node {
}

case class Plane() extends Node {
}

case class PresenceConstraint() extends Node {
}

case class PrimitiveFieldName() extends Node {
}

case class Quadruple() extends Node {
}

case class RealType() extends Node with BuiltinTypeKind {
}

case class RealValue() extends Node {
}

case class Reference() extends Node {
}

case class ReferencedObjects() extends Node {
}

case class ReferencedType() extends Node with TypeKind {
}

case class ReferencedValue() extends Node {
}

case class RelativeOidComponents() extends Node {
}

case class RelativeOidType() extends Node with BuiltinTypeKind {
}

case class RelativeOidValue() extends Node {
}

case class RequiredToken() extends Node {
}

case class RestrictedCharacterStringType() extends Node {
}

case class RestrictedCharacterStringValue() extends Node {
}

case class Root() extends Node {
}

case class RootAlternativeTypeList(namedTypes: List[NamedType]) extends Node {
}

case class RootComponentTypeList() extends Node {
}

case class RootElementSetSpec() extends Node {
}

case class RootEnumeration() extends Node {
}

case class Row() extends Node {
}
