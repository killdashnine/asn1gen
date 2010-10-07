package org.asn1gen.parsing.asn1.ast

case class Default[+T](value: T) extends Node with OptionalDefault[T] {
}

case class DefaultSyntax(
  fieldSettings: List[FieldSetting]
) extends Node with ObjectDefn {
}

trait DefinedObject
  extends ObjectKind
  with ReferencedObjects {
}

trait DefinedObjectClass
  extends GovernorKind
  with ObjectClass
  with ActualParameter
  with UserDefinedConstraintParameter {
}

trait DefinedObjectSet
  extends ObjectSetElementsKind
  with ReferencedObjects {
}

case class DefinedSyntax(
  definedSyntaxTokens: List[DefinedSyntaxToken]
) extends Node with ObjectDefn {
}

trait DefinedSyntaxToken {
}

trait DefinedType extends ReferencedType {
}

trait DefinedValue
  extends AssignedIdentifier
  with CharsDefn
  with ClassNumber
  with ExceptionIdentification
  with ObjIdComponents
  with NamedNumberValue
  with NumberForm
  with ReferencedValue
  with RelativeOidComponentsKind
  with NamedBitKind {
}

case class DefinitiveIdentifier(
  definitiveObjectIdComponents: Option[List[DefinitiveObjectIdComponent]]
) extends Node {
}

case class DefinitiveNameAndNumberForm(
  identifier: Identifier,
  definitiveNumberForm: DefinitiveNumberForm
) extends Node with DefinitiveObjectIdComponent {
}

case class DefinitiveNumberForm(
  number: Number
) extends Node with DefinitiveObjectIdComponent {
}

trait DefinitiveObjectIdComponent {
}

case class DummyGovernor(
  dummyReference: DummyReference
) extends Node with ParamGovernorKind {
}

case class DummyReference(
  reference: Reference
) extends Node {
}

