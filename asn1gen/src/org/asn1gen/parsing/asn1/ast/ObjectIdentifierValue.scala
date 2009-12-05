package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ObjectIdentifierValue(
    definedValue: Option[DefinedValue],
    objIdComponentsList: List[ObjIdComponents]
) extends Node with AssignedIdentifierKind with BuiltinValue {
}
