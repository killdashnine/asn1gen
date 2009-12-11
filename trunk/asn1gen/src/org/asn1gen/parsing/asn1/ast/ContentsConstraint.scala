package org.asn1gen.parsing.asn1.ast

case class ContentsConstraint(
  _type : Option[Type],
  value: Option[Value]
) extends Node with GeneralConstraint {
}
