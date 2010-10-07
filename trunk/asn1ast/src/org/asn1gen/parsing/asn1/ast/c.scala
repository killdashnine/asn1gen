package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class CString(
  chars: String
) extends Node
  with RestrictedCharacterStringValue
  with CharsDefn {
}

case class Cell(
  number: Number
) extends Node {
}



case class CharacterStringList(
  charsDefns: List[CharsDefn]
) extends Node
  with RelativeOidComponentsKind
  with RestrictedCharacterStringValue {
}

trait CharacterStringType extends BuiltinType {
}

trait CharacterStringValue extends BuiltinValue {
}

trait CharsDefn {
}

case class ChoiceType(typeLists: AlternativeTypeLists) extends Node with BuiltinType {
}

case class ChoiceValue(
  identifier: Identifier,
  value: Value
) extends Node with BuiltinValue {
}

trait ClassNumber {
}

trait Class_ {
}

case class Comment() extends Node {
}


case class ComponentConstraint(
  valueConstraint: ValueConstraint,
  presenceConstraint: PresenceConstraint
) extends Node {
}


case class ComponentIdList(identifiers: List[Identifier]) extends Node {
}


case class ComponentRelationConstraint(
  definedObjectSet: DefinedObjectSet,
  atNotiations: List[AtNotation]
) extends Node with TableConstraint {
}

trait ComponentType extends ExtensionAddition {
}

case class ComponentTypeList(
  componentTypes: List[ComponentType]
) extends Node with RootComponentTypeList {
}

case class ComponentTypeLists(
  list1: Option[ComponentTypeList],
  extension: Option[ComponentTypeListsExtension],
  list2: Option[ComponentTypeList]
) extends Node with SetTypeSpec with SequenceTypeSpec {
}

case class ComponentTypeListsExtension(
  extensionAndException: ExtensionAndException,
  extensionsAdditions: ExtensionsAdditions,
  optionalExtensionMarker: OptionalExtensionMarker
) {
}

case class ConstrainedType(
  typeWithConstraint: TypeWithConstraint
) extends Node with TypeKind {
}

case class Constraint(
  constraintSpec: ConstraintSpec,
  exceptionSpec: ExceptionSpec
) extends Node with ValueConstraint with ConstraintOrSizeConstraint {
}

trait ConstraintOrSizeConstraint {
}

trait ConstraintSpec {
}


case class ContainedSubtype(
  includes: Boolean,
  _type: Type
) extends Node with SubtypeElementsKind {
}

case class ContentsConstraint(
  _type : Option[Type],
  value: Option[Value]
) extends Node with GeneralConstraint {
}
