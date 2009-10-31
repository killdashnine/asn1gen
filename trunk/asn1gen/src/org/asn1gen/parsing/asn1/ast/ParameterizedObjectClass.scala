package org.asn1gen.parsing.asn1.ast

import org.asn1gen.parsing.asn1.ast.kind._

case class ParameterizedObjectClass(
    definedObjectClass: DefinedObjectClass,
    actualParameterList: ActualParameterList
) extends Node with ObjectClassKind {
}
