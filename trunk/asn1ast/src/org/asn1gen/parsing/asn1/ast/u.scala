package org.asn1gen.parsing.asn1.ast

case class UElems() extends Node {
}

class UNIQUE extends Node {
}

object UNIQUE extends UNIQUE {
}

object UTCTime extends UsefulType {
}

object UTF8String extends RestrictedCharacterStringType {
}

object UnionMark extends Node {
}

case class Unions(
    intersectionsList: List[Intersections]
) extends Node with ElementSetSpec {
}

object Universal extends Class_ {
}

object UniversalString extends RestrictedCharacterStringType {
}

object UnrestrictedCharacterStringType extends Node with CharacterStringType {
}

case class UnrestrictedCharacterStringValue(
  sequenceValue: SequenceValue
) extends Node with CharacterStringValue {
}

case class UpperEndPoint(
    exclusive: Boolean,
    upperEndValue: UpperEndValue
) extends Node {
}

trait UpperEndValue {
}

trait UsefulObjectClassReference extends DefinedObjectClass {
}

trait UsefulType extends ReferencedType {
}

case class UserDefinedConstraint(
  parameters: List[UserDefinedConstraintParameter]
) extends Node with GeneralConstraint {
}

trait UserDefinedConstraintParameter {
}
