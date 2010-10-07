package org.asn1gen.parsing.asn1.ast

trait GeneralConstraint extends ConstraintSpec {
}

object GeneralString extends RestrictedCharacterStringType {
}

object GeneralizedTime extends UsefulType {
}

case class GlobalModuleReference(
  moduleReference: ModuleReference,
  assignedIdentifier: AssignedIdentifier
) extends Node {
  def name = moduleReference.name
}

case class Governor(
  kind: GovernorKind
) extends Node with ParamGovernorKind {
}

case class GovernorConstraintParameter(
  governor: Governor,
  value: GovernorConstraintParameterValue
) extends UserDefinedConstraintParameter {
}

trait GovernorConstraintParameterValue {
}

trait GovernorKind {
}

object GraphicString extends RestrictedCharacterStringType {
}

case class Group(
  number: Number
) extends Node {
}

