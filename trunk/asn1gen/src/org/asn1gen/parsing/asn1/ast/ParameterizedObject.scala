package org.asn1gen.parsing.asn1.ast

case class ParameterizedObject(
  definedObject: DefinedObject,
  actualParameterList: ActualParameterList
) extends Node {
}
