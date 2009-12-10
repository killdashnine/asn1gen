package org.asn1gen.parsing.asn1.ast

case class Unions(
    intersectionsList: List[Intersections]
) extends Node with ElementSetSpec {
}
