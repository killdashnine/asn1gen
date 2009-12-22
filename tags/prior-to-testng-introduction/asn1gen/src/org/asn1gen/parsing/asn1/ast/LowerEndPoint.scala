package org.asn1gen.parsing.asn1.ast

case class LowerEndPoint(
    exclusive: Boolean,
    lowerEndValue: LowerEndValue
) extends Node {
}

