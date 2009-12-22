package org.asn1gen.parsing.asn1.ast

case class ParameterizedObjectClass(
    definedObjectClass: DefinedObjectClass,
    actualParameterList: ActualParameterList
) extends Node with ObjectClass {
}
