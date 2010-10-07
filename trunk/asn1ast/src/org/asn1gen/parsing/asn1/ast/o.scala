package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

trait ObjIdComponents {
}

case class ObjectAssignment(
  objectReference: ObjectReference,
  definedObjectClass: DefinedObjectClass,
  object_ : Object_
) extends Node with Assignment {
}

trait ObjectClass {
}

case class ObjectClassAssignment(
  objectClassReference: ObjectClassReference,
  objectClass: ObjectClass
) extends Node with Assignment {
}

case class ObjectClassDefn(
  fieldSpecs: List[FieldSpec],
  withSyntaxSpec: WithSyntaxSpec
) extends Node with ObjectClass {
}

case class ObjectClassFieldType(
  definedObjectClass: DefinedObjectClass,
  fieldName: FieldName
) extends Node with BuiltinType {
}


case class ObjectClassFieldValue(
  kind: ObjectClassFieldValueKind
) extends Node with BuiltinValue {
}

case class ObjectClassReference(
  chars: String
) extends Node with Reference with DefinedObjectClass {
  def name = chars
}


trait ObjectDefn extends ObjectKind {
}

object ObjectDescriptor extends UsefulType {
}


case class ObjectFieldReference(
  valueFieldReference: ValueFieldReference
) extends Node with PrimitiveFieldNameKind {
}


case class ObjectFieldSpec(
  objectFieldReference: ObjectFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectOptionalitySpec: ObjectOptionalitySpec
) extends Node with FieldSpec {
}


case class ObjectFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ObjectKind {
}


object ObjectIdentifierType extends Node with BuiltinType {
}

case class ObjectIdentifierValue(
    definedValue: Option[DefinedValue],
    objIdComponentsList: List[ObjIdComponents]
) extends Node with AssignedIdentifier with BuiltinValue {
}

case class ObjectOptionalitySpec(
  value: OptionalDefault[Object_]
) extends Node {
}


case class ObjectReference(
  chars: String
) extends Node with Reference with DefinedObject {
  def name = chars
}

case class ObjectSet(
  objectSetSpec: ObjectSetSpec
) extends Node
  with Setting
  with ActualParameter
  with GovernorConstraintParameterValue {
}

case class ObjectSetAssignment(
  objectSetReference: ObjectSetReference,
  definedObjectClass: DefinedObjectClass,
  objectSet: ObjectSet
) extends Node with Assignment {
}


case class ObjectSetElements(
  kind: ObjectSetElementsKind
) extends Node with Elements {
}


case class ObjectSetFieldReference(
  chars: String
) extends Node with PrimitiveFieldNameKind {
  def name = chars
}


case class ObjectSetFieldSpec(
  objectSetFieldReference: ObjectSetFieldReference,
  definedObjectClass: DefinedObjectClass,
  objectSetOptionalitySpec: ObjectSetOptionalitySpec
) extends Node with FieldSpec {
}


case class ObjectSetFromObjects(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ObjectSetElementsKind {
}



case class ObjectSetOptionalitySpec(
  value: OptionalDefault[ObjectSet]
) extends Node {
}

case class ObjectSetReference(
  chars: String
) extends Node with Reference with DefinedObjectSet {
  def name = chars
}

case class ObjectSetSpec(
    rootElementSetSpec: Option[RootElementSetSpec],
    additionalElementSetSpec: Option[AdditionalElementSetSpec]
) extends Node {
}



case class Object_(
  kind: ObjectKind
) extends Node
  with ActualParameter
  with GovernorConstraintParameterValue
  with ObjectSetElementsKind
  with Setting {
}

object OctetStringType extends Node with BuiltinType {
}

trait OctetStringValue extends BuiltinValue {
}

case class OpAssignment() extends Node {
}



case class OpenTypeFieldVal(
  _type: Type,
  value: Value
) extends Node with ObjectClassFieldValueKind {
}

case class Operator(
  chars: String
) extends Node {
  def name = chars
}


object Optional extends Node
  with OptionalDefault[Nothing]
  with PresenceConstraint {
}

trait OptionalDefault[+T] {
}

case class OptionalExtensionMarker(
    exists: Boolean
) extends Node {
}


case class OptionalGroup(
  tokenOrGroupSpecs: List[TokenOrGroupSpec]
) extends Node with TokenOrGroupSpec {
}
