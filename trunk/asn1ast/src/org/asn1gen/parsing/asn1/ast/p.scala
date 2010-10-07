package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object PLUS_INFINITY extends SpecialRealValue {
}

case class ParamGovernor(
  kind: ParamGovernorKind
) extends Node {
}

trait ParamGovernorKind {
}

case class Parameter(
    paramGovernor: Option[ParamGovernor],
    dummyReference: DummyReference
) extends Node {
}

case class ParameterList(
  parameters: List[Parameter]
) extends Node {
}

trait ParameterizedAssignment extends Assignment {
}

case class ParameterizedObject(
  definedObject: DefinedObject,
  actualParameterList: ActualParameterList
) extends Node with ObjectKind with ReferencedObjects {
}

case class ParameterizedObjectAssignment(
  objectReference: ObjectReference,
  parameterList: ParameterList,
  definedObjectClass: DefinedObjectClass,
  object_ : Object_
) extends Node with ParameterizedAssignment {
}

case class ParameterizedObjectClass(
    definedObjectClass: DefinedObjectClass,
    actualParameterList: ActualParameterList
) extends Node with ObjectClass {
}

case class ParameterizedObjectClassAssignment(
  objectClassReference: ObjectClassReference,
  parameterList: ParameterList,
  objectClass: ObjectClass
) extends Node with ParameterizedAssignment {
}

case class ParameterizedObjectSet(
  definedObjectSet: DefinedObjectSet,
  actualParameterList: ActualParameterList
) extends Node with ObjectSetElementsKind with ReferencedObjects {
}

case class ParameterizedObjectSetAssignment(
  objectSetReference: ObjectSetReference,
  parameterList: ParameterList,
  definedObjectClass: DefinedObjectClass,
  objectSet: ObjectSet
) extends Node with ParameterizedAssignment {
}

case class ParameterizedReference(
  reference: Reference,
  hasBraces: Boolean
) extends Node with Symbol {
}

case class ParameterizedType(
  simpleDefinedType: SimpleDefinedType,
  actualParameterList: ActualParameterList
) extends Node with DefinedType {
}

case class ParameterizedTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  _type: Type
) extends Node with ParameterizedAssignment {
}

case class ParameterizedValue(
  simpleDefinedValue: SimpleDefinedValue,
  actualParameterList: ActualParameterList
) extends Node with DefinedValue {
}

case class ParameterizedValueAssignment(
  valueReference: ValueReference,
  parameterList: ParameterList,
  _type: Type,
  value: Value
) extends Node with ParameterizedAssignment {
}

case class ParameterizedValueSetType(
  simpleDefinedType: SimpleDefinedType,
  actualParameterList: ActualParameterList
) extends Node with DefinedType {
}

case class ParameterizedValueSetTypeAssignment(
  typeReference: TypeReference,
  parameterList: ParameterList,
  _type: Type,
  valueSet: ValueSet
) extends Node with ParameterizedAssignment {
}

case class PartialSpecification(
  typeConstraints: TypeConstraints
) extends Node with MultipleTypeConstraints {
}

case class PatternConstraint(
  value: Value
) extends Node with SubtypeElementsKind {
}

case class PermittedAlphabet(
  constraint: Constraint
) extends Node with SubtypeElementsKind {
}

case class Plane(
  number: Number
) extends Node {
}

trait PresenceConstraint {
}

trait PresenceConstraintKind {
}

object Present extends PresenceConstraint {
}

case class PrimitiveFieldName(
  kind: PrimitiveFieldNameKind
) extends Node with RequiredToken {
}

trait PrimitiveFieldNameKind {
}

object PrintableString extends RestrictedCharacterStringType {
}

object Private extends Class_ {
}
