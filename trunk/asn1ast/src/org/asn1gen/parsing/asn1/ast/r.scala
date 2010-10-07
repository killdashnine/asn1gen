package org.asn1gen.parsing.asn1.ast

object REAL extends Node with BuiltinType {
}

trait RealValue extends BuiltinValue {
}

trait Reference extends Symbol {
}

trait ReferencedObjects {
}

trait ReferencedType extends TypeKind {
}

trait ReferencedValue
  extends ObjectClassFieldValueKind
  with FixedTypeFieldValKind
  with Value {
}

case class RelativeOidComponents(
  kind: RelativeOidComponentsKind
) extends Node {
}

trait RelativeOidComponentsKind {
}

object RelativeOidType extends Node with BuiltinType {
}

case class RelativeOidValue(
  relativeOidComponentsList: List[RelativeOidComponents]
) extends Node with BuiltinValue {
}

trait RequiredToken extends TokenOrGroupSpec {
}

trait RestrictedCharacterStringType extends CharacterStringType {
}

trait RestrictedCharacterStringValue extends Node with CharacterStringValue {
}

case class Root() extends Node {
}

case class RootAlternativeTypeList(alternativeTypeList: AlternativeTypeList) extends Node {
  def namedTypes = alternativeTypeList.namedTypes
}

trait RootComponentTypeList {
}

case class RootElementSetSpec(elementSetSpec: ElementSetSpec) extends Node {
}

case class RootEnumeration(
  enumeration: Enumeration
) extends Node {
}

case class Row(
  number: Number
) extends Node {
}

