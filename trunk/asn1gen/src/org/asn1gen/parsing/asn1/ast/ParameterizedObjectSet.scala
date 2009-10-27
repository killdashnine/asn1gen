package org.asn1gen.parsing.asn1.ast

case class ParameterizedObjectSet(
  definedObjectSet: DefinedObjectSet,
  actualParameterList: ActualParameterList
) extends Node {
}
