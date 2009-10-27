package org.asn1gen.parsing.asn1.ast

case class ComponentRelationConstraint(
  definedObjectSet: DefinedObjectSet,
  atNotiations: List[AtNotation]
) extends Node {
}

