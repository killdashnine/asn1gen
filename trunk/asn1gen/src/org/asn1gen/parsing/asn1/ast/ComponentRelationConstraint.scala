package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ComponentRelationConstraint(
  definedObjectSet: DefinedObjectSet,
  atNotiations: List[AtNotation]
) extends Node with TableConstraintKind {
}
