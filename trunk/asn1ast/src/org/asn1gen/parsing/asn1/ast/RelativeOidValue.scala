package org.asn1gen.parsing.asn1.ast

case class RelativeOidValue(
  relativeOidComponentsList: List[RelativeOidComponents]
) extends Node with BuiltinValue {
}
