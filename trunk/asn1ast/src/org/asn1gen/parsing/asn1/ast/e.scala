package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object ENUMERATED {
}

object EXTERNAL extends Node with BuiltinType {
}

trait ElementSetSpec extends Elements {
}

case class ElementSetSpecs(
  root: RootElementSetSpec,
  additional: Option[Option[AdditionalElementSetSpec]]
) extends Node with ConstraintSpec {
}


trait Elements {
}

case class Elems() extends Node {
}


object EmbeddedPdvType extends Node with BuiltinType {
}

case class EmbeddedPdvValue(
  sequenceValue: SequenceValue
) extends Node with BuiltinValue {
}


case class Empty() extends Node
  with AssignedIdentifier
  with Class_
  with ExceptionSpec
  with ExtensionAdditionAlternatives
  with OptionalDefault[Nothing]
  with PresenceConstraint
  with SequenceTypeSpec
  with TaggedKind
  with TagDefault
  with ValueConstraint
  with WithSyntaxSpec {
}

object Empty extends Empty() {
}

case class EnumeratedType(
  enumerations: Enumerations
) extends Node with BuiltinType {
}

case class EnumeratedValue(
  identifier: Identifier
) extends Node with BuiltinValue {
}

case class Enumeration(
  enumerationItems: List[EnumerationItem]
) extends Node {
}


trait EnumerationItem {
}

case class Enumerations(
  rootEnumeration: RootEnumeration,
  extension: Option[EnumerationsExtension1]
) extends Node {
}

case class EnumerationsExtension1(
  exceptionSpec: ExceptionSpec,
  extension: Option[EnumerationsExtension2]
) extends Node {
}

case class EnumerationsExtension2(
  additionalEnumeration: AdditionalEnumeration
) extends Node {
}

trait ExceptionIdentification extends ExceptionSpec {
}

case class ExceptionIdentificationTypeAndValue(
  _type: Type,
  value: Value
) extends ExceptionIdentification {
}

trait ExceptionSpec {
}

case class Exclusions(
  elements: Elements
) extends Node with ElementSetSpec {
}


object Explicit extends TaggedKind with TagDefault {
}

case class Exports(
  symbols: Option[SymbolsExported]
) extends Node {
}

trait ExtensionAddition {
}

trait ExtensionAdditionAlternative {
}

trait ExtensionAdditionAlternatives {
}

case class ExtensionAdditionAlternativesList(
  extensionAdditionAlternatives: List[ExtensionAdditionAlternative]
) extends Node with ExtensionAdditionAlternatives {
}


case class ExtensionAdditionGroup(
  componentTypeList: ComponentTypeList
) extends Node with ExtensionAddition {
}

case class ExtensionAdditionGroupAlternatives(
    alternativeTypeList: AlternativeTypeList
) extends Node with ExtensionAdditionAlternative {
}

case class ExtensionAdditionList(
  extensionAdditions: List[ExtensionAddition]
) extends Node {
}


case class ExtensionAndException(
    exceptionSpec: Option[ExceptionSpec]
) extends Node {
}


case class ExtensionDefault(
  set: Boolean
) extends Node {
}


case class ExtensionEndMarker() extends Node {
}


case class ExtensionSequenceTypeSpec(
  extensionAndException: ExtensionAndException,
  optionalExtensionMarker: OptionalExtensionMarker
) extends SequenceTypeSpec {
}


case class ExtensionsAdditions(
  extensionAdditionList: Option[ExtensionAdditionList]
) extends Node {
}

case class ExternalObjectClassReference(
  moduleReference: ModuleReference,
  objectClassReference: ObjectClassReference
) extends Node with DefinedObjectClass {
}

case class ExternalObjectReference(
  moduleReference: ModuleReference,
  objectReference: ObjectReference
) extends Node with DefinedObject {
}

case class ExternalObjectSetReference(
  moduleReference: ModuleReference,
  objectSetReference: ObjectSetReference
) extends Node with DefinedObjectSet {
}

case class ExternalTypeReference(
  moduleReference: ModuleReference,
  typeReference: TypeReference
) extends Node with DefinedType with SimpleDefinedType {
}

case class ExternalValue(
  sequenceValue: SequenceValue
) extends Node with BuiltinValue {
}

case class ExternalValueReference(
  moduleReference: ModuleReference,
  valueReference: ValueReference
) extends Node with DefinedValue with SimpleDefinedValue {
}
