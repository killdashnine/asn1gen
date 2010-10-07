package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

object T61String extends RestrictedCharacterStringType {
}

object TRUE extends BooleanValue(true) {
}

object TYPE_IDENTIFIER extends UsefulObjectClassReference {
}

case class TableColumn(
  number: Number
) extends Node {
}


trait TableConstraint extends GeneralConstraint {
}

case class TableRow(
  number: Number
) extends Node {
}


case class Tag(_class: Class_, classNumber: ClassNumber) extends Node {
}


trait TagDefault {
}


case class TaggedType(
  tag: Tag,
  taggedKind: TaggedKind,
  _type: Type
) extends Node with BuiltinType {
  def typeKind = _type.kind
  def constraints = _type.constraints
}

case class TaggedValue(
  value: Value
) extends Node with BuiltinValue {
}

object TeletexString extends RestrictedCharacterStringType {
}

trait TokenOrGroupSpec {
}

case class Tuple(
  tableColumn: TableColumn,
  tableRow: TableRow
) extends Node with RestrictedCharacterStringValue with CharsDefn {
}


case class Type(
  kind: TypeKind,
  constraints: List[Constraint]
) extends Node
    with ActualParameter
    with GovernorKind
    with Setting
    with UserDefinedConstraintParameter {
}

case class TypeAssignment(
  typeReference: TypeReference,
  _type: Type
) extends Node with Assignment {
  def name = typeReference.name
}


case class TypeConstraint(
  _type: Type
) extends Node with SubtypeElementsKind {
}

case class TypeConstraints(namedConstraints: List[NamedConstraint]) extends Node {
}



case class TypeFieldReference(
  chars: String
) extends Node with PrimitiveFieldNameKind {
  def name = chars
}


case class TypeFieldSpec(
  typeFieldReference: TypeFieldReference,
  typeOptionalitySpec: TypeOptionalitySpec
) extends Node with FieldSpec {
}

case class TypeFromObject(
  referencedObjects: ReferencedObjects,
  fieldName: FieldName
) extends Node with ReferencedType {
}

trait TypeKind {
}

case class TypeOptionalitySpec(
    value: OptionalDefault[Type]
) extends Node {
}

case class TypeReference(
  name: String
) extends Node
    with Reference
    with DefinedType
    with SimpleDefinedType {
  def asModuleReference = ModuleReference(name)
}

case class TypeWithConstraint(
  typeSpec: SequenceOrSet,
  constraintSpec: ConstraintOrSizeConstraint,
  _type: Type
) extends Node {
}
