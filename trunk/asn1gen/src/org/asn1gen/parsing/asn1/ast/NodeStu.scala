package org.asn1gen.parsing.asn1.ast

case class SelectionType() extends Node {
}

case class SequenceOfType() extends Node with BuiltinTypeKind {
}

case class SequenceOfValue() extends Node {
}

case class SequenceType() extends Node with BuiltinTypeKind {
}

case class SequenceValue() extends Node {
}

case class SetOfType() extends Node with BuiltinTypeKind {
}

case class SetOfValue() extends Node {
}

case class SetType() extends Node with BuiltinTypeKind {
}

case class SetValue() extends Node {
}

case class Setting() extends Node {
}

case class SignedNumber(negative: Boolean, magnitude: Number) extends Node with NamedNumberValue {
}

case class SimpleDefinedType() extends Node {
}

case class SimpleDefinedValue() extends Node {
}

case class SizeConstraint() extends Node {
}

case class SpecialRealValue() extends Node {
}

case class SubtypeElements() extends Node {
}

case class Symbol() extends Node {
}

case class SymbolsExported() extends Node {
}

case class SymbolsFromModule() extends Node {
}

case class SymbolsImported() extends Node {
}

case class SyntaxList() extends Node {
}

case class TableColumn() extends Node {
}

case class TableConstraint() extends Node {
}

case class TableRow() extends Node {
}

case class TagDefault() extends Node {
}

case class TaggedValue() extends Node {
}

case class TokenOrGroupSpec() extends Node {
}

case class Tuple() extends Node {
}

case class TypeAssignment(name: TypeReference, `type`: Type_) extends Node with AssignmentKind {
}

case class TypeFieldReference(chars : String) extends Node {
  def name = chars
}

case class TypeFieldSpec() extends Node {
}

case class TypeFromObject() extends Node {
}

case class TypeOptionalitySpec() extends Node {
}

case class TypeReference(chars : String) extends Node {
  def name = chars
  def asModuleReference = ModuleReference(chars)
}

case class TypeWithConstraint() extends Node {
}

case class UElems() extends Node {
}

case class UnionMark() extends Node {
}

case class Unions() extends Node {
}

case class Unique() extends Node {
}

case class UnrestrictedCharacterStringType() extends Node {
}

case class UpperEndPoint() extends Node {
}

case class UpperEndValue() extends Node {
}

case class UsefulObjectClassReference() extends Node {
}

case class UsefulType() extends Node {
}

case class UserDefinedConstraintParameter() extends Node {
}
