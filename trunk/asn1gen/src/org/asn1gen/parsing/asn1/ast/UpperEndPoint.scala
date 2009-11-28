package org.asn1gen.parsing.asn1.ast

case class UpperEndPoint(
    exclusive: Boolean,
    upperEndValue: UpperEndValue
) extends Node {
}
