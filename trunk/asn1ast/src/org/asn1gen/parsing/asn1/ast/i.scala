package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object IA5String extends RestrictedCharacterStringType {
}

case class IElems() extends Node {
}


case class INTEGER(
  namedNumbers: Option[List[NamedNumber]]
) extends Node with BuiltinType {
}

object ISO646String extends RestrictedCharacterStringType {
}

case class Identifier(
  name: String
) extends Node with IntegerValue with EnumerationItem {
}

case class IdentifierList(
  identifiers: List[Identifier]
) extends Node with BitStringValue {
}


object Implicit extends TaggedKind with TagDefault {
}

case class Imports(
  symbols: Option[SymbolsImported]
) extends Node {
}

case class Includes() extends Node {
}



trait InnerTypeConstraints extends SubtypeElementsKind {
}

case class InstanceOfType(
  definedObjectClass: DefinedObjectClass
) extends Node with BuiltinType {
}

case class InstanceOfValue(
  value: Value
) extends Node with BuiltinValue {
}

trait IntegerValue extends BuiltinValue with EnumerationItem {
}

case class IntersectionElements(
    elements: Elements,
    exclusions: Option[Exclusions]
) extends Node {
}

object IntersectionMark extends Node {
}


case class Intersections(
    intersectionElementsList: List[IntersectionElements]
) extends Node {
}

