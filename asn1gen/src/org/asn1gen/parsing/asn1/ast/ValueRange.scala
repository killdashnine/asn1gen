package org.asn1gen.parsing.asn1.ast

case class ValueRange(
  lowerEndPoint: LowerEndPoint,
  upperEndPoint: UpperEndPoint
) extends Node {
}

