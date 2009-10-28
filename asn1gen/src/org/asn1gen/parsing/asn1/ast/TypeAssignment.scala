package org.asn1gen.parsing.asn1.ast

case class TypeAssignment(name: TypeReference, `type`: Type_) extends Node with AssignmentKind {
}

