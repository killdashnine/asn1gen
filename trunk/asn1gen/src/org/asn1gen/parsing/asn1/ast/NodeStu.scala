package org.asn1gen.parsing.asn1.ast

case class SelectionType() extends Node {
}

case class SequenceOfType() extends Node {
}

case class SequenceOfValue() extends Node {
}

case class SequenceType() extends Node {
}

case class SequenceValue() extends Node {
}

case class SetOfType() extends Node {
}

case class SetOfValue() extends Node {
}

case class SetType() extends Node {
}

case class SetValue() extends Node {
}

case class Setting() extends Node {
}

case class SignedNumber() extends Node {
}

case class SimpleDefinedType() extends Node {
}

case class SimpleDefinedValue() extends Node {
}

case class SimpleTableConstraint() extends Node {
}

case class SingleTypeConstraint() extends Node {
}

case class SingleValue() extends Node {
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

case class Tag() extends Node {
}

case class TagDefault() extends Node {
}

case class TaggedType() extends Node {
}

case class TaggedValue() extends Node {
}

case class TokenOrGroupSpec() extends Node {
}

case class Tuple() extends Node {
}

case class Type() extends Node {
}

case class TypeAssignment() extends Node {
}

case class TypeConstraint() extends Node {
}

case class TypeConstraints() extends Node {
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

case class UnrestrictedCharacterStringValue() extends Node {
}

case class UpperEndPoint() extends Node {
}

case class UpperEndValue() extends Node {
}

case class UsefulObjectClassReference() extends Node {
}

case class UsefulType() extends Node {
}

case class UserDefinedConstraint() extends Node {
}

case class UserDefinedConstraintParameter() extends Node {
}
