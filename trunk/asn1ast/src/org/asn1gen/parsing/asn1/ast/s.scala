package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class SelectionType(
  identifier: Identifier,
  _type: Type
) extends Node with ReferencedType {
}

object Sequence extends SequenceOrSet {
}

case class SequenceOfType(
  _type: Type
) extends Node with BuiltinType {
}

case class SequenceOfValue(
  values: List[Value]
) extends Node with BuiltinValue {
}

trait SequenceOrSet {
}

case class SequenceType(
  spec: SequenceTypeSpec
) extends Node with BuiltinType {
}

trait SequenceTypeSpec {
}


case class SequenceValue(
  namedValues: List[NamedValue]
) extends Node with BuiltinValue with NumericRealValue {
}

object Set extends SequenceOrSet {
}

case class SetOfType(
  _type: Type
) extends Node with BuiltinType {
}

case class SetOfValue(
  values: List[Value]
) extends Node with BuiltinValue {
}

case class SetType(
  spec: SetTypeSpec
) extends BuiltinType {
}


case class SetTypeExtension(
  extensionAndException: ExtensionAndException,
  optionalExtensionMarker: OptionalExtensionMarker
) extends Node with SetTypeSpec {
}

trait SetTypeSpec {
}

case class SetValue(
  namedValues: List[NamedValue]
) extends Node with BuiltinValue {
}

trait Setting extends DefinedSyntaxToken {
}

case class SignedNumber(
  negative: Boolean,
  magnitude: Number
) extends Node
    with NamedNumberValue
    with IntegerValue
    with ExceptionIdentification {
}

trait SimpleDefinedType {
}

trait SimpleDefinedValue {
}

case class SimpleTableConstraint(
  objectSet: ObjectSet
) extends Node with TableConstraint {
}

case class SingleTypeConstraint(
  constraint: Constraint
) extends Node with InnerTypeConstraints {
}



case class SingleValue(
  value: Value
) extends Node with SubtypeElementsKind {
}



case class SizeConstraint(
  constraint: Constraint
) extends Node
  with SubtypeElementsKind
  with ConstraintOrSizeConstraint {
}

class SpecialRealValue extends Node with RealValue {
}


case class SubtypeElements(
  kind: SubtypeElementsKind
) extends Node with Elements {
}

trait Symbol {
}

case class SymbolsExported(
  symbols: List[Symbol]
) extends Node {
}


case class SymbolsFromModule(
  symbols: List[Symbol],
  globalModuleReference: GlobalModuleReference
) extends Node {
  def module = globalModuleReference.name
}

case class SymbolsImported(
  symbolsFromModuleList: List[SymbolsFromModule]
) extends Node {
}

case class SyntaxList(
  tokenOrGroupSpecs: List[TokenOrGroupSpec]
) extends Node with WithSyntaxSpec {
}
