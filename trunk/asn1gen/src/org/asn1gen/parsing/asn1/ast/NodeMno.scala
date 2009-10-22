package org.asn1gen.parsing.asn1.ast

case class ModuleBody(exports : Exports, imports : Imports, assignmentList: AssignmentList) extends Node {
  def this() = {this(Exports(), Imports(), AssignmentList(Nil))}
}

case class ModuleDefinition(
  identifier : ModuleIdentifier,
  tagDefault : TagDefault,
  extensionDefault : ExtensionDefault,
  moduleBody : ModuleBody
) extends Node {
  def name = identifier.name
}

case class ModuleIdentifier(reference : ModuleReference, definitiveIdentifier : DefinitiveIdentifier) extends Node {
  def name = reference.name
}

case class ModuleReference(chars : String) extends Node {
  def name = chars
}

case class MultipleTypeConstraints() extends Node {
}

case class NameAndNumberForm() extends Node {
}

case class NameForm() extends Node {
}

case class NamedBit() extends Node {
}

case class NamedConstraint() extends Node {
}

case class NamedNumber(identifier: Identifier, value: NamedNumberValue) extends Node {
  def name = identifier.name
}

case class NamedType() extends Node {
}

case class NamedValue() extends Node {
}

case class NonZeroNumber() extends Node {
}

case class NullType() extends Node {
}

case class NullValue() extends Node {
}

case class Number(chars : String) extends Node {
  def negative = Number("-" + chars)
}

case class NumberForm() extends Node {
}

case class NumericRealValue() extends Node {
}

case class ObjIdComponents() extends Node {
}

case class Object() extends Node {
}

case class ObjectAssignment() extends Node {
}

case class ObjectClass() extends Node {
}

case class ObjectClassAssignment() extends Node {
}

case class ObjectClassDefn() extends Node {
}

case class ObjectClassFieldType() extends Node {
}

case class ObjectClassFieldValue() extends Node {
}

case class ObjectClassReference(chars : String) extends Node {
  def name = chars
}

case class ObjectDefn() extends Node {
}

case class ObjectFieldReference(chars : String) extends Node {
  def name = chars
}

case class ObjectFieldSpec() extends Node {
}

case class ObjectFromObject() extends Node {
}

case class ObjectIdentifierType() extends Node {
}

case class ObjectIdentifierValue() extends Node {
}

case class ObjectOptionalitySpec() extends Node {
}

case class ObjectReference() extends Node {
}

case class ObjectSet() extends Node {
}

case class ObjectSetAssignment() extends Node {
}

case class ObjectSetElements() extends Node {
}

case class ObjectSetFieldReference(chars : String) extends Node {
  def name = chars
}

case class ObjectSetFieldSpec() extends Node {
}

case class ObjectSetFromObjects() extends Node {
}

case class ObjectSetOptionalitySpec() extends Node {
}

case class ObjectSetReference(chars : String) extends Node {
  def name = chars
}

case class ObjectSetSpec() extends Node {
}

case class OctetStringType() extends Node {
}

case class OctetStringValue() extends Node {
}

case class OpAssignment() extends Node {
}

case class OpenTypeFieldVal() extends Node {
}

case class Operator(chars : String) extends Node {
  def name = chars
}

case class OptionalExtensionMarker() extends Node {
}

case class OptionalGroup() extends Node {
}
