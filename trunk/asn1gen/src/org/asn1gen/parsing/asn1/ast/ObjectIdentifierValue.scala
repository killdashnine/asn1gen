package org.asn1gen.parsing.asn1.ast

case class ObjectIdentifierValue(
    definedValue: Option[DefinedValue],
    objIdComponentsList: List[ObjIdComponents]
) extends Node with AssignedIdentifier with BuiltinValue {
}
