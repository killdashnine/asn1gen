package org.asn1gen.parsing.asn1.ast

case class ContentsConstraint(
  type_ : Option[Type_],
  value: Option[Value]
) extends Node with GeneralConstraint {
}
