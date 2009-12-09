package org.asn1gen.parsing.asn1.ast

case class ComponentConstraint(
  valueConstraint: ValueConstraint,
  presenceConstraint: PresenceConstraint
) extends Node {
}

